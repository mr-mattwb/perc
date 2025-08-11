open Unix
open Printf
open Stdlib

open Tools
open Env
open Log

type return_code = int

module BuildCommand = Cmd(
    struct
        let name = "commands.build"
        let default = "/usr/bin/sox"
        let switches = ["-b"; "--build-cmd" ]
        let desc = "Conversion command to use"
    end)
module DurCommand = Cmd(
    struct
        let name = "commands.duration"
        let default = "/usr/bin/soxi -D"
        let switches = ["-d"; "--dur-cmd"]
        let desc = "Return the duration of a sound file in seconds"
    end)
module PlayCommand = Cmd(
    struct
        let name = "commands.play"
        let default = "/usr/bin/play"
        let switches = ["-P"; "--play-cmd"]
        let desc = "Command to play a sound file"
    end)
module OutFile = File(
    struct
        let name = "percolate.outfile"
        let default = "out.wav"
        let switches = ["-o"; "--out-file"]
        let desc = "Output filename"
    end)
module Seconds = Int(
    struct
        let name = "percolate.seconds"
        let default = 20
        let switches = ["-s"; "--seconds"]
        let desc = "Seconds of percolation."
    end) 
module Play = Set(
    struct
        let name = "percolate.play"
        let default = false
        let switches = ["-p"; "--play"]
        let desc = "Play the output file"
    end)

module FileExt = Str(
    struct
        let name = "percolate.extension"
        let default = ".wav"
        let switches = ["--file-extension"]
        let desc = "File extension to use for any output files"
    end)

module PercFile = Option(File(
    struct
        let name = "percolate.soundfile"
        let default = ""
        let switches = ["--sound-file"]
        let desc = "Percolator file"
    end))

module LogLevel = Log.Level
module LogTargets = Log.Targets
module LogName(N : Log.NAME) = Log.Named(
    struct
        let mod_name = 
            match N.mod_name with
            | "" -> "PercCfg"
            | n -> "PercCfg:"^n
    end)
module Verbose = 
    struct
        include Env.Verbose
        let set_verbose () = 
            LogLevel.put Debug;
            ignore (LogTargets.add (Channel stderr))
        let clear_verbose () =
            LogLevel.put Warn;
            ignore (LogTargets.remove (Channel stderr))
        let configure () =
            if get() then set_verbose()
            else clear_verbose()
    end

module type ELT = 
    sig
        val file_duration : file -> return_code
        val play_file : unit -> return_code
        val build_file : seconds ->  file -> return_code
        val with_percolator_file : (file -> 'a) -> 'a
    end

module PLog = LogName(struct let mod_name = "" end)
let file_duration fname = 
    let cmd = sprintf "%s %s" (DurCommand.get()) fname in
    PLog.debug "Duration [%s]" cmd;
    let get fin = 
        match input_line fin with
        | None ->
            PLog.error "Command [%s] did not return a response" cmd;
            0
        | Some line ->
            PLog.info "Duration [%s] => [%s]" cmd line;
            Tools.ceil_of_string line
    in
    Tools.with_in_process get cmd

let play_file () = 
    let cmd = sprintf "%s %s" (PlayCommand.get()) (OutFile.get()) in
    PLog.debug "Play [%s]" cmd;
    let rsp = Sys.command cmd in
    PLog.info "Play [%s] => [%d]" cmd rsp;
    rsp

let build_file seconds percfile = 
    let rec aux count cmd = 
        match count with
        | c when c <= 0 -> cmd^" "^(OutFile.get())
        | _ -> aux (count - seconds) (cmd^" "^percfile)
    in
    let cmd = aux (Seconds.get()) (BuildCommand.get()) in
    PLog.debug "Build [%s]" cmd;
    let rsp = Sys.command cmd in
    PLog.info "Build [%s] => [%d]" cmd rsp;
    rsp

let with_contents contents fn = 
    Tools.with_temp_file "" (FileExt.get()) (fun percFile ->
        Tools.put_file percFile contents;
        PLog.info "Percolator temp file [%s]" percFile;
        fn percFile)

let with_percolator_file fn =
    let contents = 
        match PercFile.get() with
        | None -> Perc5sWav.percolate
        | Some fname -> Tools.get_file fname
    in with_contents contents fn
    



