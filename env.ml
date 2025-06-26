open Unix
open Printf
open Stdlib

open Tools

module type ELT =
    sig
        type elt
        val name : string
        val descr : string
        val switch : string
        val arg : Arg.key * Arg.spec * Arg.doc
        val get : unit -> elt
        val put : elt -> unit
    end
module type FILE_ELT = ELT with type elt = file

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

type unixflag = string
let gSkipArgs = "SKIP_ARGS"
let gSkipArgsIfInteractive = "SKIP_ARGS_IF_INTERACTIVE"

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
    if not (unix_get_flag gSkipArgs) then gProgramArgs := StrMap.add name arg !gProgramArgs;
    if not (unix_get_flag gSkipArgsIfInteractive) then
        if not !Sys.interactive then 
            gProgramArgs := StrMap.add name arg !gProgramArgs
        

module Make(S : Ser.ELT)(P : PARAMS with type elt = S.elt) : ELT with type elt = P.elt =
    struct
        type elt = P.elt
        let name = P.name
        let descr = P.descr
        let switch = P.switch
        let arg = 
            (P.switch, Arg.String (fun v -> Unix.putenv name v), 
                sprintf "[%s][%s] %s" P.name (S.to_string P.default) P.descr)
        let get () = 
            try S.of_string (Unix.getenv name) 
            with e -> P.default
        let put v = Unix.putenv name (S.to_string v)
        let () = add_program_arg name arg
    end


module MakeStr(P : STR_PARAMS) = Make(Ser.Str)(struct type elt = string include P end)
module MakeInt(P : INT_PARAMS) = Make(Ser.Int)(struct type elt = int include P end)
module MakeFlt(P : FLT_PARAMS) = Make(Ser.Flt)(struct type elt = float include P end)
module MakeBool(P : BOOL_PARAMS) = Make(Ser.Bool)(struct type elt = bool include P end)
module Set(P : BOOL_PARAMS) =
    struct
        include MakeBool(
            struct
                type elt = bool
                include P
            end)
        let arg = (P.switch, Arg.Unit (fun () -> (Unix.putenv P.name "true")), sprintf "[%s][%b] %s" P.name P.default P.descr)
        let () = add_program_arg name arg
    end
module Clear(P : BOOL_PARAMS) =
    struct
        include MakeBool(
            struct
                type elt = bool
                include P
            end)
        let arg = (P.switch, Arg.Unit (fun () -> (Unix.putenv P.name "false")), sprintf "[%s][%b] %s" P.name P.default P.descr)
        let () = add_program_arg name arg
    end

module MakeFile(P : FILE_PARAMS) = Make(Ser.Str)(
    struct
        type elt = file
        let name = P.name
        let descr = P.descr
        let default = P.default
        let switch = P.switch
    end)

module CfgFile = MakeFile(
    struct
        let name = "CONFIGFILE"
        let descr = "Name of the configuration file"
        let default = (Filename.basename Sys.argv.(0))^".cfg"
        let switch = "--cfg-file"
    end)


let try_load_config_file () = 
    try
        let fname = CfgFile.get() in
        Lex.load_file fname
    with e ->
        eprintf "try_load_config_file [%s]\n%!" (Printexc.to_string e)


let config () =
    try_load_config_file ();
    arg_default()

    



