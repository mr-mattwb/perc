open Unix
open Printf
open Stdlib

type file = string

module type ELT =
    sig
        type elt
        val name : string
        val descr : string
        val arg : Arg.key * Arg.spec * Arg.doc
        val get : unit -> elt
        val put : elt -> unit
    end
module type FILE_ELT = ELT with type elt = file

module type SERIAL = 
    sig
        type elt 
        val of_string : string -> elt
        val to_string : elt -> string
    end

module type STR_PARAMS = 
    sig
        val default : string
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
module StrSet = Set.Make(
    struct
        type t = arg
        let compare (a1, b1, c1) (a2, b2, c2) = String.compare a1 a2 
    end)

let gProgramArgs = ref StrSet.empty
let gHelp = ref false
let args anons msg = Arg.parse (StrSet.elements !gProgramArgs) anons msg
let parse_args msg = 
    let invalid_arg v = 
        eprintf "%s:  Invalid argument [%s]\n%!" Sys.argv.(0) v;
        gHelp := true
    in
    args invalid_arg msg
let arg_default () = parse_args "Invalid argument"

let unix_get_flag f = try bool_of_string (Unix.getenv f) with _ -> false

module Make(S : SERIAL)(P : PARAMS with type elt = S.elt) : ELT with type elt = P.elt =
    struct
        type elt = P.elt
        let name = P.name
        let descr = P.descr
        let arg = 
            (P.switch, Arg.String (fun v -> Unix.putenv name v), 
                sprintf "[%s][%s] %s" P.name (S.to_string P.default) P.descr)
        let get () = 
            try S.of_string (Unix.getenv name) with e -> P.default
        let put v = Unix.putenv name (S.to_string v)

        let _ = 
            if not (unix_get_flag gSkipArgs) then gProgramArgs := StrSet.add arg !gProgramArgs;
            if not (unix_get_flag gSkipArgsIfInteractive) then
                if not !Sys.interactive then gProgramArgs := StrSet.add arg !gProgramArgs
    end

module StrSer =
    struct
        type elt = string
        let of_string v = v
        let to_string v = v
    end
module IntSer = 
    struct
        type elt = int
        let of_string v = int_of_string (String.trim v)
        let to_string = string_of_int
    end
module FltSer = 
    struct
        type elt = float
        let of_string v = float_of_string (String.trim v)
        let to_string = string_of_float
    end
module BoolSer = 
    struct
        type elt = bool
        let of_string = bool_of_string
        let to_string = string_of_bool
    end


module MakeStr(P : PARAMS with type elt = string) = Make(StrSer)(P)
module MakeInt(P : PARAMS with type elt = int) = Make(IntSer)(P)
module MakeFlt(P : PARAMS with type elt = float) = Make(FltSer)(P)
module MakeBool(P : PARAMS with type elt = bool) = Make(BoolSer)(P)


module MakeFile(P : FILE_PARAMS) = Make(StrSer)(
    struct
        type elt = file
        let name = P.name
        let descr = P.descr
        let default = P.default
        let switch = P.switch
    end)

module LogFile = MakeFile(
    struct
        let name = "LOG_FILE"
        let descr = "Name of the log file"
        let default = "file.log"
        let switch = "--log-file"
    end)

module CfgFile = MakeFile(
    struct
        let name = "CONFIG_FILE"
        let descr = "Name of the configuration file"
        let default = "file.cfg"
        let switch = "--cfg-file"
    end)


let try_load_config_file () = 
    try
        let fname = CfgFile.get() in
        CfgLex.load_file fname
    with e ->
        eprintf "try_load_config_file [%s]\n%!" (Printexc.to_string e)


let config () =
    try_load_config_file ();
    arg_default()

    



