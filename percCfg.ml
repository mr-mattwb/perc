open Unix
open Printf
open Stdlib

open Tools
open Env
open Log

module BuildCommand = Str(
    struct
        type elt = string
        let name = "BUILDCOMMAND"
        let default = "/usr/bin/sox"
        let switch = "--build-command"
        let descr = "Conversion command to use"
    end)
module DurCommand = Str(
    struct
        type elt = string
        let name = "DURCOMMAND"
        let default = "/usr/bin/soxi -D"
        let switch = "--dur-command"
        let descr = "Return the duration of a sound file in seconds"
    end)
module PlayCommand = Str(
    struct
        type elt = string
        let name = "PLAYCOMMAND"
        let default = "/usr/bin/play"
        let switch = "--play-command"
        let descr = "Command to play a sound file"
    end)
module OutFile = File(
    struct
        type elt = file
        let name = "OUTFILE"
        let default = "out.wav"
        let switch = "--out-file"
        let descr = "Output filename"
    end)
module Seconds = Int(
    struct
        type elt = int
        let name = "SECONDS"
        let default = 20
        let switch = "--seconds"
        let descr = "Seconds of percolation."
    end) 
module LogLevel = Log.LevelEnv(
    struct
        let name = "LOGLEVEL"
        let default = Debug
        let switch = "--log-level"
        let descr = "Min log level"
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

module PLog = Log.Make(
    struct
        let mod_name = "PercCfg"
        let level = Debug
        let targets = [Channel stderr]
    end)

let file_duration fname = 
    let cmd = sprintf "%s %s" (DurCommand.get()) fname in
    PLog.debug "File duration command [%s]" cmd;
    let get fin = 
        match input_line fin with
        | None ->
            PLog.error "Command [%s] did not return a response" cmd;
            0
        | Some line ->
            PLog.info "Command [%s] => [%s]" cmd line;
            int_of_float (float_of_string line)
    in
    Tools.with_in_process get cmd

let play_file () = 
    let cmd = sprintf "%s %s" (PlayCommand.get()) (OutFile.get()) in
    PLog.debug "Play command [%s]" cmd;
    let rsp = Sys.command cmd in
    PLog.info "Command [%s] => [%d]" cmd rsp;
    rsp

let build_file seconds percfile = 
    let rec aux count cmd = 
        if count <= 0 then
            cmd^" "^(OutFile.get())
        else
            aux (count - seconds) (cmd^" "^percfile)
    in
    let cmd = aux (Seconds.get()) (BuildCommand.get()) in
    PLog.debug "Command [%s]" cmd;
    let rsp = Sys.command cmd in
    PLog.info "Command [%s] => [%d]" cmd rsp;
    rsp

let with_percolator_file fn = 
    Tools.with_temp_file "" (FileExt.get()) (fun percFile ->
        Tools.put_file percFile Perc5sWav.percolate;
        PLog.info "Percolator temp file [%s]" percFile;
        fn percFile)



