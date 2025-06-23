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

let rec main () = 
    CfgEnv.config ();
    let log = CfgLog.makeb Err in
    CfgLog.write log "%s [%s]" Command.name (Command.get());
    CfgLog.write log "%s [%s]" Play.name (Play.get());
    CfgLog.write log "%s [%s]" PercFile.name (PercFile.get());
    CfgLog.write log "%s [%s]" OutFile.name (OutFile.get());
    CfgLog.write log "%s [%d]" Seconds.name (Seconds.get());
    CfgLog.write log "%s [%d]" Iterator.name (Iterator.get())
;;

if not !Sys.interactive then
    try 
        main ();
        exit (0)
    with e ->
        eprintf "[%s] Fatal Error [%s]" Sys.argv.(0) (Printexc.to_string e);
        exit (-1)
;;
