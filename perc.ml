open Env
open Log
open PercCfg

open Unix
open Printf
open Stdlib
open Arg

let rec main () = 
    Env.config ();
    let module CLog = Log.MakeSub(
        struct
            let mod_name = "main"
            let level = LogLevel.get()
            let targets = [Channel stderr]
        end)
    in
    CLog.debug "%s [%s] [%s]" BuildCommand.name BuildCommand.switch (BuildCommand.get());
    CLog.debug "%s [%s]" DurCommand.name (DurCommand.get());
    CLog.debug "%s [%s]" PlayCommand.name (PlayCommand.get());
    CLog.warn "%s [%s]" PercFile.name (PercFile.get());
    CLog.error "%s [%s]" OutFile.name (OutFile.get());
    CLog.warn "%s [%d]" Seconds.name (Seconds.get());
    CLog.debug "%s [%s]" LogLevel.name (LevelSer.to_string (LogLevel.get()));
    CLog.debug "%s [%s]" LogFile.name (LogFile.get());
    CLog.debug "%s [%b]" PlayResult.name (PlayResult.get());
    run ()
and run () = 
    let module RLog = Log.MakeSub(
        struct
            let mod_name = "run"
            let level = Debug
            let targets = [Channel stderr]
        end)
    in
    let length = file_duration (DurCommand.get()) (PercFile.get()) in
    RLog.debug "Percolate file size [%d]" length;
    let rsp = build_file (Seconds.get()) length (BuildCommand.get()) (PercFile.get()) (OutFile.get()) in
    RLog.debug "Build_file [%s] => [%d]" (OutFile.get()) rsp;
    let rsp = 
        if (PlayResult.get()) then
            play_file (PlayCommand.get()) (OutFile.get())
        else
            0
    in
    RLog.debug "Played file [%d]" rsp
;;

if not !Sys.interactive then
    try 
        main ();
        exit (0)
    with e ->
        eprintf "[%s] Fatal Error [%s]\n%!" Sys.argv.(0) (Printexc.to_string e);
        exit (-1)
;;
