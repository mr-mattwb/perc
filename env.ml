open Unix
open Printf
open Stdlib

open Tools

type cfg = 
    | Properties
    | Ini

module type PARAMS = 
    sig
        val name : string
        val desc : string
        val switches : string list
    end
module type DEFPARAMS = 
    sig
        type elt
        val default : elt
        include PARAMS
    end
module type STR_PARAMS = 
    sig
        val default : string
        include PARAMS
    end 
module type INT_PARAMS = 
    sig
        val default : int
        include PARAMS
    end
module type INT32_PARAMS = 
    sig
        val default : int32
        include PARAMS
    end
module type INT64_PARAMS = 
    sig
        val default : int64
        include PARAMS
    end
module type FLT_PARAMS = 
    sig
        val default : float
        include PARAMS
    end
module type BOOL_PARAMS = 
    sig
        val default : bool
        include PARAMS
    end
module type FLAG_PARAMS = PARAMS

module type FILE_PARAMS = STR_PARAMS
module type CMD_PARAMS = STR_PARAMS
module type MULTI_PARAMS = PARAMS

module type ELT =
    sig
        include DEFPARAMS
        val of_string : string -> elt
        val to_string : elt -> string
        val args : (Arg.key * Arg.spec * Arg.doc) list
        val get : unit -> elt
        val put : elt -> unit
    end
module type STR_ELT = ELT with type elt = string
module type INT_ELT = ELT with type elt = int
module type INT32_ELT = ELT with type elt = int32
module type INT64_ELT = ELT with type elt = int64
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
        val touch : unit -> unit
        val mkdir : perms -> unit
    end
module type CMD_ELT = 
    sig
        include ELT with type elt = cmd
        val run : unit -> return_code
        val run_args : cmd -> return_code
        val with_in : (in_channel -> 'a) -> string -> 'a 
    end

module type MULTI_ELT =
    sig
        type t
        include ELT with type elt = t list
        val add : t -> unit
    end


type unixflag = string
let gSkipArgs = "SKIP_ARGS"
let gAllowOverride = "ALLOW_OVERRIDE"
let gCfgType = "CONFIG_TYPE"

type arg = Arg.key * Arg.spec * Arg.doc
module SwMap = Map.Make(String)

let gProgramArgs = ref SwMap.empty
let gHelp = ref false
let args anons msg = 
    let elts = List.map (fun (sw, arg) -> arg) (SwMap.bindings !gProgramArgs) in
    Arg.parse elts anons msg
let parse_args msg = 
    let invalid_arg v = 
        eprintf "%s:  Invalid argument [%s]\n%!" Sys.argv.(0) v;
        gHelp := true
    in
    args invalid_arg msg
let arg_default () = parse_args "Invalid argument"

let unix_get_flag f = try bool_of_string (Unix.getenv f) with _ -> false
let add_program_arg ?(override=false) name args =
    if not (unix_get_flag gSkipArgs) then 
        List.iter (fun (sw, spec, desc) ->
            match SwMap.find_opt sw !gProgramArgs with
            | None -> 
                gProgramArgs := SwMap.add sw (sw, spec, desc) !gProgramArgs
            | Some arg when unix_get_flag gAllowOverride || override -> 
                gProgramArgs := SwMap.add sw (sw, spec, desc) (SwMap.remove sw !gProgramArgs)
            | Some _ ->
                raise (Failure (sprintf "Name[%s] Switch[%s] already exists" name sw)))
            args

let getConfigType () = 
    match String.uppercase_ascii (Unix.getenv gCfgType) with
    | "INI" -> Ini
    | "PROPERTIES" -> Properties
    | unknown ->
        eprintf "%s:  Unknown Configuration type [%s].  Default to INI\n%!" Sys.argv.(0) unknown;
        Ini

module NameMap = 
    struct
        include Map.Make(String)
        let replace key item map = add key item (remove key map)
    end
let gNameMap = ref NameMap.empty
let add_name name (put : string -> unit) = 
    match NameMap.find_opt name !gNameMap with
    | None -> gNameMap := NameMap.add name put !gNameMap
    | Some _ -> gNameMap := NameMap.replace name put !gNameMap

module Make(S : Ser.ELT)(P : DEFPARAMS with type elt = S.elt) : ELT with type elt = P.elt =
    struct
        include S
        let name = P.name
        let desc = P.desc
        let default = P.default
        let switches = P.switches
        let args = 
            List.map (fun switch -> 
                (switch, Arg.String (fun v -> Unix.putenv name v), 
                    sprintf "[%s][%s] %s" P.name (S.to_string P.default) P.desc))
                switches
        let get () = 
            try S.of_string (Unix.getenv name) 
            with e -> P.default
        let put v = Unix.putenv name (S.to_string v)
        let () = 
            add_program_arg name args;
            add_name name (fun (x : string) -> Unix.putenv name x)
    end

module OList = List
module List(S : Ser.ELT)(P : DEFPARAMS with type elt = S.elt list) : ELT with type elt  = P.elt = Make(Ser.List(S))(P)

module Hide(E : ELT) = 
    struct
        include E
        let () = 
            OList.iter (fun switch -> gProgramArgs := SwMap.remove switch !gProgramArgs ) E.switches
    end

module Str(P : STR_PARAMS) = Make(Ser.Str)(struct type elt = string include P end)
module StrEmpty(P : PARAMS) = Str(
    struct
        type elt = string
        include P
        let default = ""
    end)
module Int(P : INT_PARAMS) = Make(Ser.Int)(struct type elt = int include P end)
module Int32(P : INT32_PARAMS) = Make(Ser.Int32)(struct type elt = int32 include P end)
module Int64(P : INT64_PARAMS) = Make(Ser.Int64)(struct type elt = int64 include P end)
module Int_0(P : PARAMS) = Int(
    struct
        type elt = int
        include P
        let default = 0
    end)
module Flt(P : FLT_PARAMS) = Make(Ser.Flt)(struct type elt = float include P end)
module Flt_0(P : PARAMS) = Flt(
    struct
        type elt = float
        include P
        let default = 0.
    end)
module Bool(P : BOOL_PARAMS) = Make(Ser.Bool)(struct type elt = bool include P end)
module Set(P : FLAG_PARAMS) =
    struct
        include Bool(
            struct
                type elt = bool
                include P
                let default = false
            end)
        let args = 
            OList.map (fun switch ->
                (switch, Arg.Unit (fun () -> (Unix.putenv P.name "true")), sprintf "[%s][%b] %s" P.name default P.desc))
                switches
        let () = add_program_arg ~override:true name args
    end
module Clear(P : FLAG_PARAMS) =
    struct
        include Bool(
            struct
                type elt = bool
                include P
                let default = true
            end)
        let args = 
            OList.map (fun switch -> 
                (switch, Arg.Unit (fun () -> (Unix.putenv P.name "false")), sprintf "[%s][%b] %s" P.name default P.desc))
                switches
        let () = add_program_arg ~override:true name args
    end

module File(P : FILE_PARAMS) = 
    struct
        module F = Make(Ser.Str)(
            struct
                type elt = file
                let name = P.name
                let desc = P.desc
                let default = P.default
                let switches = P.switches
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
        let touch () = 
            let fname = get() in
            if Sys.file_exists fname && (stat fname).st_kind = S_DIR then
                Unix.closedir (Unix.opendir fname)
            else
                Tools.with_out_file flush fname
        let mkdir perms = Unix.mkdir (get()) perms
    end

module Cmd(P : CMD_PARAMS) =
    struct
        include Str(P)
        let run () = Sys.command (get())
        let run_args args = Sys.command ((get())^" "^args)
        let with_in fn args = Tools.with_in_process fn ((get())^args)
    end

module CfgFile = Hide(File(
    struct
        let name = "CONFIGFILE"
        let desc = "Name of the configuration file"
        let default = (Filename.basename Sys.argv.(0))^".cfg"
        let switches = ["--cfg-file"]
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
        let default = 
            match S.to_string S.default with
            | "" -> None
            | _ -> Some S.default
        let switches = S.switches
        let desc = S.desc
        let args = S.args
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
        let switches = S.switches
        let desc = S.desc
        let args = S.args
        let get () = 
            match S.get () with
            | n when n = N.none -> None
            | n -> Some n
        let put = function
            | None -> S.put N.none
            | Some s -> S.put s
    end


module MultiValue(S : Ser.ELT)(P : PARAMS) : MULTI_ELT with type t = S.elt =
    struct
        let name = P.name
        let switches = P.switches
        let desc = P.desc
        let default = []
        module LS = List(S)(
            struct
                type elt = S.elt list
                let name = P.name
                let switches = P.switches
                let desc = P.desc
                let default = []
            end)
        let of_string = LS.of_string
        let to_string = LS.to_string
        type t = S.elt
        type elt = t list
        let rec get () = try LS.of_string (Unix.getenv name) with Not_found -> default
        and put v = Unix.putenv name (LS.to_string v)

        let add item = put (item :: (get()))

        let addstr item = 
            add (S.of_string item)
        let do_switch sw = 
            (sw, Arg.String (fun item -> addstr item), 
                sprintf "[%s][%s] %s" name (to_string default) desc)
        let args = OList.map do_switch P.switches
        let () = 
            add_program_arg name args;
            add_name name (fun str -> put ((S.of_string str)::(get())))
    end

module Verbose = Set(
    struct
        let name = "verbose"
        let default = false
        let switches = [ "-v"; "--verbose" ]
        let desc = "At a minimum, turn on DEBUG logging."
    end)

open PropBase

let rec try_config_file () = 
    try parse_config (CfgFile.get())
    with Not_found -> eprintf "Config file not found\n%!"
and parse_config fname = 
    match getConfigType () with
    | Ini -> with_lex_file (main None (IniParse.main IniLex.main)) fname
    | Properties -> with_lex_file (main None (PropParse.main PropLex.main)) fname
and with_lex_file fn fname = Tools.with_in_file (fun fin -> fn (Lexing.from_channel fin)) fname
and main ctx parse lex = 
    match parse lex with
    | Eof -> ()
    | Context "" -> main None parse lex
    | Context c -> main (Some c) parse lex
    | Pair (k, v) ->
        let k' = 
            match ctx with
            | None -> k
            | Some c -> c^"."^k
        in
        newpair k' v;
        main ctx parse lex
and newpair name v = 
    match NameMap.find_opt name !gNameMap with
    | None -> Unix.putenv name v
    | Some put -> put v

let config () =
    try_config_file ();
    arg_default ()
