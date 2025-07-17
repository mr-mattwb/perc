open Unix
open Printf
open Stdlib

open Tools

module type STR_PARAMS = 
    sig
        val default : string
        val name : string
        val descr : string
        val switch : string
    end 
module type INT_PARAMS = 
    sig
        val default : int
        val name : string
        val descr : string
        val switch : string
    end
module type FLT_PARAMS = 
    sig
        val default : float
        val name : string
        val descr : string
        val switch : string
    end
module type BOOL_PARAMS = 
    sig
        val default : bool
        val name : string
        val descr : string
        val switch : string
    end
module type PARAMS = 
    sig
        type elt
        val default : elt
        val name : string
        val descr : string
        val switch : string
    end 
module type FILE_PARAMS = STR_PARAMS
module type CMD_PARAMS = STR_PARAMS

module type ELT =
    sig
        include PARAMS
        val of_string : string -> elt
        val to_string : elt -> string
        val arg : Arg.key * Arg.spec * Arg.doc
        val get : unit -> elt
        val put : elt -> unit
    end
module type STR_ELT = ELT with type elt = string
module type INT_ELT = ELT with type elt = int
module type FLT_ELT = ELT with type elt = float
module type BOOL_ELT = ELT with type elt = bool
module type FILE_ELT = 
    sig
        include ELT with type elt = file
        val exists : unit -> bool
        val file : unit -> file option
        val base : unit -> file
        val dir : unit -> dir
        val is_dir : unit -> bool
    end
module type CMD_ELT = 
    sig
        include ELT with type elt = cmd
        val run : unit -> return_code
        val run_args : cmd -> return_code
    end

type unixflag = string
let gSkipArgs = "SKIP_ARGS"
let gAllowOverride = "ALLOW_OVERRIDE"

type arg = Arg.key * Arg.spec * Arg.doc
module StrMap = Map.Make(String)

let gProgramArgs = ref StrMap.empty
let gHelp = ref false
let args anons msg = 
    let elts = List.map (fun (sw, arg) -> arg) (StrMap.bindings !gProgramArgs) in
    Arg.parse elts anons msg
let parse_args msg = 
    let invalid_arg v = 
        eprintf "%s:  Invalid argument [%s]\n%!" Sys.argv.(0) v;
        gHelp := true
    in
    args invalid_arg msg
let arg_default () = parse_args "Invalid argument"

let unix_get_flag f = try bool_of_string (Unix.getenv f) with _ -> false
let add_program_arg name arg =
    if not (unix_get_flag gSkipArgs) then 
        match StrMap.find_opt name !gProgramArgs with
        | None -> 
            gProgramArgs := StrMap.add name arg !gProgramArgs
        | Some arg when unix_get_flag gAllowOverride -> 
            gProgramArgs := StrMap.add name arg (StrMap.remove name !gProgramArgs)
        | Some _ ->
            raise (Failure (sprintf "Switch[%s] already exists" name))

module Make(S : Ser.ELT)(P : PARAMS with type elt = S.elt) : ELT with type elt = P.elt =
    struct
        include S
        let name = P.name
        let descr = P.descr
        let default = P.default
        let switch = P.switch
        let arg = 
            (P.switch, Arg.String (fun v -> Unix.putenv name v), 
                sprintf "[%s][%s] %s" P.name (S.to_string P.default) P.descr)
        let get () = 
            try S.of_string (Unix.getenv name) 
            with e -> P.default
        let put v = Unix.putenv name (S.to_string v)
        let () = add_program_arg switch arg
    end

module List(S : Ser.ELT)(P : PARAMS with type elt = S.elt list) : ELT with type elt  = P.elt = Make(Ser.List(S))(P)

module Hide(E : ELT) = 
    struct
        include E
        let () = gProgramArgs := StrMap.remove E.name !gProgramArgs 
    end

module Str(P : STR_PARAMS) = Make(Ser.Str)(struct type elt = string include P end)
module Int(P : INT_PARAMS) = Make(Ser.Int)(struct type elt = int include P end)
module Flt(P : FLT_PARAMS) = Make(Ser.Flt)(struct type elt = float include P end)
module Bool(P : BOOL_PARAMS) = Make(Ser.Bool)(struct type elt = bool include P end)
module Set(P : BOOL_PARAMS) =
    struct
        include Bool(
            struct
                type elt = bool
                include P
            end)
        let arg = (P.switch, Arg.Unit (fun () -> (Unix.putenv P.name "true")), sprintf "[%s][%b] %s" P.name P.default P.descr)
        let () = add_program_arg switch arg
    end
module Clear(P : BOOL_PARAMS) =
    struct
        include Bool(
            struct
                type elt = bool
                include P
            end)
        let arg = (P.switch, Arg.Unit (fun () -> (Unix.putenv P.name "false")), sprintf "[%s][%b] %s" P.name P.default P.descr)
        let () = add_program_arg switch arg
    end

module File(P : FILE_PARAMS) = 
    struct
        module F = Make(Ser.Str)(
            struct
                type elt = file
                let name = P.name
                let descr = P.descr
                let default = P.default
                let switch = P.switch
            end)
        include F
        let exists () = Sys.file_exists (F.get())
        let file () = 
            if exists() then Some (get())
            else None
        let base () = Filename.basename (get())
        let dir () = Filename.dirname (get())
        let is_dir () = 
            match file() with
            | None -> false
            | Some f -> (stat f).st_kind = S_DIR
    end

module Cmd(P : CMD_PARAMS) =
    struct
        include Str(P)
        let run () = Sys.command (get())
        let run_args args = Sys.command ((get())^" "^args)
    end

module CfgFile = Hide(File(
    struct
        let name = "CONFIGFILE"
        let descr = "Name of the configuration file"
        let default = (Filename.basename Sys.argv.(0))^".cfg"
        let switch = "--cfg-file"
    end))

module Option(S : ELT) = 
    struct
        type elt = S.elt option
        let of_string = function
            | "" -> None

            | str -> Some (S.of_string str)
        let to_string = function
            | None -> ""
            | Some str -> S.to_string str
        let name = S.name
        let default = Some S.default
        let switch = S.switch
        let descr = S.descr
        let arg = S.arg
        let get () =
            match S.to_string (S.get ()) with
            | "" -> None
            | str -> Some (S.get())
        let put v = 
            match v with
            | None -> S.put (S.of_string "")
            | Some v -> S.put v
    end

module type NONE = 
    sig
        type o 
        val none : o 
    end
module MakeOption(S : ELT)(N : NONE with type o = S.elt) = 
    struct
        type elt = S.elt option
        let snone = S.to_string N.none
        let of_string str = 
            match str with
            | s when s = snone -> None
            | s -> Some (S.of_string s)
        let to_string = function
            | None -> snone
            | Some msg -> S.to_string msg
        let name = S.name
        let default  = Some S.default
        let switch = S.switch
        let descr = S.descr
        let arg = S.arg
        let get () = 
            match S.get () with
            | n when n = N.none -> None
            | n -> Some n
        let put = function
            | None -> S.put N.none
            | Some s -> S.put s
    end

let try_load_config_file () = 
    try
        let fname = CfgFile.get() in
        Lex.load_file fname
    with e ->
        eprintf "try_load_config_file [%s]\n%!" (Printexc.to_string e)


let config () =
    try_load_config_file ();
    arg_default()

    



