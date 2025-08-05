open Unix
open Printf
open Stdlib

open Env

module BuildCommand = Cmd(
    struct
        let name = "BUILDCOMMAND"
        let default = "/usr/bin/sox"
        let desc = "Command to convert sound file"
        let switches = ["-b"; "--build-command"]
    end)
module DurCommand = Cmd(
    struct
        let name = "DURCOMMAND"
        let default = "/usr/bin/soxi -D"
        let desc = "Command to get file duration"
        let switches = ["-d"; "--dur-command"]
    end)
module PlayCommand = Cmd(
    struct
        let name = "PLAYCOMMAND"
        let default = "/usr/bin/play"
        let desc = "Command to get play a sound file"
        let switches = ["-p"; "--play-cmd"]
    end)
module OutFile = File(
    struct
        let name = "OUTFILE"
        let default = "output.ulaw"
        let desc = "Output sound file"
        let switches = ["-o"; "--out-file"]
    end)
module PercFile = File(
    struct
        let default = "percolate.ulaw"
        let desc = "Sound file that plays percolate music"
        let switches = ["-p"; "--perc-file"]
        let name = "PERCFILE"
    end)
module Seconds = Int(
    struct
        let name = "SECONDS"
        let default = 60
        let desc = "Seconds from a config file"
        let switches = ["-s"; "--seconds"]
    end)
module Play = Set(
    struct
        let name = "PLAY"
        let default = false
        let desc = "Command to play a sound file"
        let switches = ["--play"]
    end)
module FileExt = Str(
    struct
        let name = "FILEEXTENSION"
        let default = ".wav"
        let switches = ["-x"; "--file-extension"]
        let desc = "File extension to use for any output files"
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
