open Unix
open Printf
open Stdlib
module OList = List

open Env

module BuildCommand = Cmd(
    struct
        let name = "commands.build"
        let default = "/usr/bin/sox"
        let desc = "Command to convert sound file"
        let switches = ["-b"; "--build-command"]
    end)
module DurCommand = Cmd(
    struct
        let name = "commands.duration"
        let default = "/usr/bin/soxi -D"
        let desc = "Command to get file duration"
        let switches = ["-d"; "--dur-command"]
    end)
module PlayCommand = Cmd(
    struct
        let name = "commands.play"
        let default = "/usr/bin/play"
        let desc = "Command to get play a sound file"
        let switches = ["-p"; "--play-cmd"]
    end)
module OutFile = File(
    struct
        let name = "percolate.outfile"
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
        let name = "percolate.seconds"
        let default = 60
        let desc = "Seconds from a config file"
        let switches = ["-s"; "--seconds"]
    end)
module Play = Set(
    struct
        let name = "percolate.play"
        let default = false
        let desc = "Command to play a sound file"
        let switches = ["--play"]
    end)
module FileExt = Str(
    struct
        let name = "percolate.extension"
        let default = ".wav"
        let switches = ["-E"; "--file-extension"]
        let desc = "File extension to use for any output files"
    end)
module ExtraSwitches = MultiValue(Ser.Str)(
    struct
        let name = "extra.args"
        let desc = "Description"
        let switches = [ "-x" ]
    end)
module TestInt32 = Int32(
    struct
        let name = "TESTINT32"
        let default = 100l
        let desc = "Test Int32"
        let switches = ["--i32"]
    end)
module TestInt64 = Int64(
    struct
        let name = "TESTINT64"
        let default = 101L
        let desc = "Test Int64"
        let switches = ["--i64"]
    end)

let run () = 
    Env.config();
    printf "%s [%s]\n%s [%s]\n%s [%s]\n" BuildCommand.name (BuildCommand.get()) DurCommand.name (DurCommand.get())
        PlayCommand.name (PlayCommand.get());
    printf "%s [%s]\n%s [%s]\n" OutFile.name (OutFile.get()) PercFile.name (PercFile.get());
    printf "%s [%d]\n%s [%b]\n%s [%s]\n" Seconds.name (Seconds.get()) Play.name (Play.get()) FileExt.name (FileExt.get());
    printf "%s [%ldl]\n%!" TestInt32.name (TestInt32.get());
    printf "%s [%LdL]\n%!" TestInt64.name (TestInt64.get());
    try
        printf "%s [%s]\n%!" ExtraSwitches.name (Unix.getenv ExtraSwitches.name);
        printf "%s [" ExtraSwitches.name;
        match ExtraSwitches.get() with
        | [] -> printf "]\n%!"
        | x :: [] -> printf "%s]\n%!" x
        | x :: xs -> 
            printf "%s" x;
            OList.iter (fun x -> printf ",%s" x) xs;
            printf "]\n%!"
    with Not_found ->
        eprintf "ExtraSwitches triggered Not_found exception\n%!"

;;

let main () = 
    try run ()
    with e -> eprintf "%s:  Error [%s]\n%!" Sys.argv.(0) (Printexc.to_string e)
;;

if not !Sys.interactive then
    main ()
;;
