open Env
open Log
open PercCfg

open Unix
open Printf
open Stdlib
open Arg

let rec main () = 
    Env.config ();
    let module CLog = LogName(struct let mod_name = "main" end) in
    CLog.debug "%s [%s] [%s]" BuildCommand.name BuildCommand.switch (BuildCommand.get());
    CLog.debug "%s [%s]" DurCommand.name (DurCommand.get());
    CLog.debug "%s [%s]" PlayCommand.name (PlayCommand.get());
    CLog.debug "%s [%s]" OutFile.name (OutFile.get());
    CLog.debug "%s [%d]" Seconds.name (Seconds.get());
    CLog.debug "%s [%s]" LogLevel.name (LevelSer.to_string (LogLevel.get()));
    CLog.debug "%s [%b]" Play.name (Play.get());
    run ()
and run () = 
    let module RLog = LogName(struct let mod_name = "run" end) in
    PercCfg.with_percolator_file (fun percFile ->
        let length = file_duration percFile in
        RLog.debug "Percolate file size [%s] [%d]" percFile length;
        let rsp = build_file length percFile in
        RLog.debug "Build_file [%s] => [%d]" (OutFile.get()) rsp;
        let rsp = 
            if (Play.get()) then
                play_file ()
            else
                0
        in
        RLog.debug "Played file [%d]" rsp)
;;

if not !Sys.interactive then
    try 
        main ();
        exit (0)
    with e ->
        eprintf "[%s] Fatal Error [%s]\n%!" Sys.argv.(0) (Printexc.to_string e);
        exit (-1)
;;
