open Env
open CfgLog
open PercCfg

open Unix
open Printf
open Stdlib
open Arg

let rec main () = 
    Env.config ();
    let module Log = CfgLog.Make(
        struct
            let mod_name = Filename.basename Sys.argv.(0) 
            let level = LogLevel.get()
            let targets = [Channel stderr]
        end)
    in
    Log.debug "%s [%s]" Command.name (Command.get());
    Log.debug "%s [%s]" DurCommand.name (DurCommand.get());
    Log.debug "%s [%s]" PlayCommand.name (PlayCommand.get());
    Log.warn "%s [%s]" PercFile.name (PercFile.get());
    Log.error "%s [%s]" OutFile.name (OutFile.get());
    Log.warn "%s [%d]" Seconds.name (Seconds.get());
    Log.info "%s [%d]" Iterator.name (Iterator.get());
    Log.debug "%s [%s]" LogLevel.name (LevelSer.to_string (LogLevel.get()));
    Log.debug "%s [%s]" LogFile.name (LogFile.get());
    Log.debug "%s [%b]" PlayResult.name (PlayResult.get());
    run ()
and run () = 
    let module Log = CfgLog.Make(
        struct
            let mod_name = (Filename.basename Sys.argv.(0))^":run"
            let level = Debug
            let targets = [Channel stderr]
        end)
    in
    let length = file_duration (DurCommand.get()) (PercFile.get()) in
    Log.debug "Percolate file size [%d]" length;
    let rsp = build_file (Seconds.get()) length (Command.get()) (PercFile.get()) (OutFile.get()) in
    Log.debug "Build_file [%s] => [%d]" (OutFile.get()) rsp;
    let rsp = 
        if (PlayResult.get()) then
            play_file (PlayCommand.get()) (OutFile.get())
        else
            0
    in
    Log.debug "Played file [%d]" rsp
;;

if not !Sys.interactive then
    try 
        main ();
        exit (0)
    with e ->
        eprintf "[%s] Fatal Error [%s]\n%!" Sys.argv.(0) (Printexc.to_string e);
        exit (-1)
;;
