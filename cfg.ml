open Unix
open Printf
open Stdlib

open Env

module BuildCommand = Cmd(
    struct
        let name = "BUILDCOMMAND"
        let default = "/usr/bin/sox"
        let descr = "Command to convert sound file"
        let switch = "--build-command"
    end)
module DurCommand = Cmd(
    struct
        let name = "DURCOMMAND"
        let default = "/usr/bin/soxi -D"
        let descr = "Command to get file duration"
        let switch = "--"
    end)
module PlayCommand = Cmd(
    struct
        let name = "PLAYCOMMAND"
        let default = "/usr/bin/play"
        let descr = "Command to get play a sound file"
        let switch = "--play-cmd"
    end)
module OutFile = File(
    struct
        let name = "OUTFILE"
        let default = "output.ulaw"
        let descr = "Output sound file"
        let switch = "--out-file"
    end)
module PercFile = File(
    struct
        let default = "percolate.ulaw"
        let descr = "Sound file that plays percolate music"
        let switch = "--perc-file"
        let name = "PERCFILE"
    end)
module Seconds = Int(
    struct
        let name = "SECONDS"
        let default = 60
        let descr = "Seconds from a config file"
        let switch = "--seconds"
    end)
module Play = Set(
    struct
        let name = "PLAY"
        let default = false
        let descr = "Command to play a sound file"
        let switch = "--play"
    end)
module FileExt = Str(
    struct
        let name = "FILEEXTENSION"
        let default = ".wav"
        let switch = "--file-extension"
        let descr = "File extension to use for any output files"
    end)


let run () = 
    Env.config();
    printf "%s [%s]\n%s [%s]\n%s [%s]\n" BuildCommand.name (BuildCommand.get()) DurCommand.name (DurCommand.get())
        PlayCommand.name (PlayCommand.get());
    printf "%s [%s]\n%s [%s]\n" OutFile.name (OutFile.get()) PercFile.name (PercFile.get());
    printf "%s [%d]\n%s [%b]\n%s [%s]\n" Seconds.name (Seconds.get()) Play.name (Play.get()) FileExt.name (FileExt.get())

;;

let main () = 
    try run ()
    with e -> eprintf "%s:  Error [%s]\n%!" Sys.argv.(0) (Printexc.to_string e)
;;

if not !Sys.interactive then
    main ()
;;
