open Unix
open Printf
open Stdlib

open Tools
open Env
open Log

type return_code = int

module BuildCommand = Cmd(
    struct
        let name = "BUILDCOMMAND"
        let default = "/usr/bin/sox"
        let switch = "--build-command"
        let descr = "Conversion command to use"
    end)
module DurCommand = Cmd(
    struct
        let name = "DURCOMMAND"
        let default = "/usr/bin/soxi -D"
        let switch = "--dur-command"
        let descr = "Return the duration of a sound file in seconds"
    end)
module PlayCommand = Cmd(
    struct
        let name = "PLAYCOMMAND"
        let default = "/usr/bin/play"
        let switch = "--play-command"
        let descr = "Command to play a sound file"
    end)
module OutFile = File(
    struct
        let name = "OUTFILE"
        let default = "out.wav"
        let switch = "--out-file"
        let descr = "Output filename"
    end)
module Seconds = Int(
    struct
        let name = "SECONDS"
        let default = 20
        let switch = "--seconds"
        let descr = "Seconds of percolation."
    end) 
module Play = Set(
    struct
        let name = "PLAY"
        let default = false
        let switch = "--play"
        let descr = "Play the output file"
    end)

module FileExt = Str(
    struct
        let name = "FILEEXTENSION"
        let default = ".wav"
        let switch = "--file-extension"
        let descr = "File extension to use for any output files"
    end)

module PercFile = Option(File(
    struct
        let name = "PERCFILE"
        let default = ""
        let switch = "--perc-file"
        let descr = "Percolator file"
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
            Tools.tolerint_of_string line
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
        if count <= 0 then
            cmd^" "^(OutFile.get())
        else
            aux (count - seconds) (cmd^" "^percfile)
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
    



