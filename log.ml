open Unix
open Printf
open Stdlib

open Tools
open Env

type mod_name = string
type out = 
    | Channel of out_channel
    | File of Tools.file

type level =
    | Off
    | Debug
    | Info
    | Warn
    | Error
    | Fatal

module type CHAN_PARAMS =
    sig
        val mod_name : mod_name
        val level : level
    end
module type PARAMS = 
    sig
        include CHAN_PARAMS
        val targets : out list
    end

module type ELT = 
    sig
        val debug : ('a, unit, string, unit) format4 -> 'a
        val info : ('a, unit, string, unit) format4 -> 'a
        val warn : ('a, unit, string, unit) format4 -> 'a
        val error : ('a, unit, string, unit) format4 -> 'a
        val throw : exn -> ('a, unit, string, unit) format4 -> 'a
    end

let use ofn cfn ufn arg = 
    let hnd = ofn arg in
    try
        let rc = ufn hnd in
        cfn hnd;
        rc
    with e ->
        (try cfn hnd with _ -> ());
        raise e

let rec app_output_file (fname : Tools.file) fn = 
    use open_out_app close_out fn fname
and open_out_app (fname : file) = 
    open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 fname
let with_append (fname : file) fn = app_output_file fname fn

module type LEVEL_SER = Ser.ELT with type elt = level
module type LEVEL_ENV = 
    sig
        val name : string
        val default : level
        val switch : string
        val descr : string
    end 
module LevelSer = 
    struct
        type elt = level
        let to_string = function
            | Off -> "OFF"
            | Debug -> "DEBUG"
            | Info -> "INFO"
            | Warn -> "WARN"
            | Error -> "ERROR"
            | Fatal -> "FATAL"
        let of_string v =
            match String.uppercase_ascii v with
            | "OFF" -> Off
            | "DEBUG" -> Debug
            | "INFO" -> Info
            | "WARN" -> Warn
            | "ERROR" -> Error
            | "FATAL" -> Fatal
            | lvl -> raise (Failure ("level_of_string:"^lvl))
    end
module LevelEnv(LP : LEVEL_ENV) = Env.Make(LevelSer)(
    struct
        type elt = level
        include LP
    end)

let msg_string modn lvl msg =
    let tm = Unix.localtime (Unix.time()) in
    sprintf "%04d-%02d-%02d %02d:%02d:%02d %8s %8d %10s %s"
        (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday
        tm.tm_hour tm.tm_min tm.tm_sec 
        modn (getpid()) (LevelSer.to_string lvl) msg
let msg_output ch modn lvl (msg : string) = 
    fprintf ch "%s\n%!" (msg_string modn lvl msg)
let msg_out ch modn msg = 
    msg_output ch modn Off msg
let msg_outb ch msg = 
    msg_output ch (Filename.basename Sys.argv.(0)) Off msg

let to_string modn lvl fmt =
    let aux msg = msg_string modn lvl msg in
    ksprintf aux fmt

let fprintf ch modn lvl fmt =
    let aux msg = msg_output ch modn lvl msg in
    ksprintf aux fmt

let oprintf modn lvl fmt = 
    let aux msg = msg_output stdout modn lvl msg in
    ksprintf aux fmt 

let eprintf modn lvl fmt = 
    let aux msg = msg_output stderr modn lvl msg in
    ksprintf aux fmt

let file_printf fname modn lvl fmt = 
    let aux2 msg ch = msg_output ch modn lvl msg in
    let aux msg = app_output_file fname (aux2 msg) in
    ksprintf aux fmt

let buffer_printf buf modn lvl fmt = 
    let aux msg = 
        bprintf buf "%s\n%!" (msg_string modn lvl msg)
    in
    ksprintf aux fmt

module Make(P : PARAMS) =
    struct
        let ignore fmt = 
            let aux _ = () in
            ksprintf aux fmt

        let write (lvl : level) (msg : string) = function
            | File f -> with_append f (fun ch -> msg_output ch P.mod_name lvl msg)
            | Channel ch -> msg_output ch P.mod_name lvl msg
            
        let write_targets lvl fmt = 
            let aux msg = List.iter (write lvl msg) P.targets in
            ksprintf aux fmt

        let write_fatal e fmt = 
            let aux msg = 
                List.iter (write Fatal msg) P.targets;
                raise e
            in
            ksprintf aux fmt

        let debug (fmt : ('a, unit, string, unit) format4) =
            match P.level with
            | Debug -> write_targets Debug fmt
            | _ -> ignore fmt
        let info (fmt : ('a, unit, string, unit) format4) =
            match P.level with
            | Debug
            | Info -> write_targets Info fmt
            | _ -> ignore fmt
        let warn (fmt : ('a, unit, string, unit) format4) = 
            match P.level with
            | Debug
            | Info
            | Warn -> write_targets Warn fmt
            | _ -> ignore fmt
        let error (fmt : ('a, unit, string, unit) format4) = 
            match P.level with
            | Debug | Info | Warn | Error -> write_targets Error fmt
            | _ -> ignore fmt
        let throw e (fmt : ('a, unit, string, unit) format4) = 
            match P.level with
            | Off -> ignore fmt
            | _ -> write_fatal e fmt
    end

module MakeSub(P : PARAMS) = 
    struct
        include Make(
            struct
                include P
                let mod_name = Tools.basename^":"^P.mod_name
            end)
    end

module Stdout(P : CHAN_PARAMS) = Make(
    struct
        include P
        let targets = [Channel stdout]
    end)
module Stderr(P : CHAN_PARAMS) = Make(
    struct
        include P
        let targets = [Channel stderr]
    end)

let logModName = "LOGMODNAME"
let logLevel = "LOGLEVEL"
let logTarget = "LOGTARGET"
module Enviro = Make(
    struct
        (* Environments 
            LOGMODNAME
            LOGLEVEL
            LOGTARGET
        *)
        let mod_name = 
            match Tools.getenv logModName with
            | None -> Tools.basename
            | Some n -> n
        let level = 
            match Tools.getenv logLevel with
            | None -> Warn
            | Some v -> LevelSer.of_string v
        let targets =
            match Tools.getenv logTarget with
            | None -> []
            | Some "STDOUT" -> [Channel stdout]
            | Some "STDERR" -> [Channel stderr]
            | Some "FILE" -> [File (Tools.basename^".log")]
            | Some fname -> [File fname]
    end)

