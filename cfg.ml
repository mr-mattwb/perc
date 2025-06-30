open Unix
open Printf
open Stdlib

open Env

module Command = Str(
    struct
        type elt = string
        let default = "/usr/bin/sox"
        let descr = "Command to convert sound file"
        let switch = "--command"
        let name = "COMMAND"
    end)
module Play = Str(
    struct
        type elt = string
        let default = "/usr/bin/play"
        let descr = "Command to play a sound file"
        let switch = "--play"
        let name = "PLAY"
    end)
module PercFile = File(
    struct
        let default = "percolate.ulaw"
        let descr = "Sound file that plays percolate music"
        let switch = "--perc-file"
        let name = "PERCFILE"
    end)
module OutFile = File(
    struct
        let default = "output.ulaw"
        let descr = "Output sound file"
        let switch = "--out-file"
        let name = "OUTFILE"
    end)
module Seconds = Int(
    struct
        type elt = int
        let default = 60
        let descr = "Seconds from a config file"
        let switch = "--seconds"
        let name = "SECONDS"
    end)
module Iterator = Int(
    struct
        type elt = int
        let default = 60
        let descr = "Number of times to play sound file"
        let switch = "--iterator"
        let name = "ITERATOR"
    end)
;;

let run () = 
    Env.config();
    printf "%s [%s]\n%s [%s]\n%s [%s]\n%s [%s]\n%s [%d]\n%s [%d]\n%!" 
        Command.name (Command.get())
        Play.name (Play.get())
        PercFile.name (PercFile.get())
        OutFile.name (OutFile.get())
        Seconds.name (Seconds.get())
        Iterator.name (Iterator.get())
;;

let main () = 
    try run ()
    with e -> eprintf "%s:  Error [%s]\n%!" Sys.argv.(0) (Printexc.to_string e)
;;

if not !Sys.interactive then
    main ()
;;
