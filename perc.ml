open CfgEnv
open CfgLog

open Unix
open Printf
open Stdlib
open Arg


module Command = MakeStr(
    struct
        type elt = string
        let name = "COMMAND"
        let default = "/usr/bin/sox"
        let switch = "--command"
        let descr = "Conversion command to use"
    end)
module Play = MakeStr(
    struct
        type elt = string
        let name = "PLAY"
        let default = "/usr/bin/play"
        let switch = "--play"
        let descr = "Play sound files"
    end)
module PercFile = MakeFile( 
    struct
        type elt = file
        let name = "PERCFILE"
        let default = "perc-5s.wav"
        let switch = "--perc-file"
        let descr = "File containing percolation sound."
    end)
module OutFile = MakeFile(
    struct
        type elt = file
        let name = "OUTFILE"
        let default = "out.wav"
        let switch = "--out-file"
        let descr = "Output filename"
    end)
module Seconds = MakeInt(
    struct
        type elt = int
        let name = "SECONDS"
        let default = 20
        let switch = "--seconds"
        let descr = "Seconds of percolation."
    end) 
module Iterator = MakeInt(
    struct
        type elt = int
        let name = "ITERATOR"
        let default = 5
        let switch = "--iterator"
        let descr = "seconds per each percolator file" 
    end)
module LogLevel = CfgLog.LevelEnv(
    struct
        let name = "LOGLEVEL"
        let default = Debug
        let switch = "--log-level"
        let descr = "Min log level"
    end)
module LogFile = MakeFile(
    struct
        let name = "LOGFILE"
        let default = (Filename.basename Sys.argv.(0))^".log"
        let switch = "--log-file"
        let descr = "Log file name"
    end)

let rec main () = 
    eprintf "LogLevel [%s]\n%!" (LevelSer.to_string (LogLevel.get()));
    CfgEnv.config ();
    let module Log = CfgLog.Make(
        struct
            let mod_name = Filename.basename Sys.argv.(0) 
            let level = LogLevel.get()
            let targets = [Channel stderr]
        end)
    in
    Log.debug "%s [%s]" Command.name (Command.get());
    Log.info "%s [%s]" Play.name (Play.get());
    Log.warn "%s [%s]" PercFile.name (PercFile.get());
    Log.error "%s [%s]" OutFile.name (OutFile.get());
    Log.warn "%s [%d]" Seconds.name (Seconds.get());
    Log.info "%s [%d]" Iterator.name (Iterator.get());
    Log.debug "%s [%s]" LogLevel.name (LevelSer.to_string (LogLevel.get()));
    Log.debug "%s [%s]" LogFile.name (LogFile.get());
    run (Command.get())
and run start = 
    let module Log = CfgLog.Make(
        struct
            let mod_name = (Filename.basename Sys.argv.(0))^":run"
            let level = Debug
            let targets = [Channel stderr]
        end)
    in
    let rec aux cmd count = 
        if count <= 0 then
            cmd^" "^(OutFile.get())
        else
            aux (cmd^" "^(PercFile.get())) (count - (Iterator.get()))
    in
    let cmd = aux start (Seconds.get()) in
    let rsp = Sys.command cmd in
    Log.info "Command [%s] => [%d]" cmd rsp
;;

if not !Sys.interactive then
    try 
        main ();
        exit (0)
    with e ->
        eprintf "[%s] Fatal Error [%s]" Sys.argv.(0) (Printexc.to_string e);
        exit (-1)
;;
