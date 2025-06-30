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
module PercFile = File( 
    struct
        type elt = file
        let name = "PERCFILE"
        let default = "perc-5s.wav"
        let switch = "--perc-file"
        let descr = "File containing percolation sound."
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
module LogFile = File(
    struct
        let name = "LOGFILE"
        let default = (Filename.basename Sys.argv.(0))^".log"
        let switch = "--log-file"
        let descr = "Log file name"
    end)
module Play = Set(
    struct
        let name = "PLAY"
        let default = false
        let switch = "--play"
        let descr = "Play the output file"
    end)

module PLog = Log.MakeSub(
    struct
        let mod_name = "PercCfg"
        let level = Debug
        let targets = [Channel stderr]
    end)

let file_duration durcommand fname = 
    let cmd = sprintf "%s %s" durcommand fname in
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

let play_file playcmd outf = 
    let cmd = sprintf "%s %s" playcmd outf in
    PLog.debug "Play command [%s]" cmd;
    let rsp = Sys.command cmd in
    PLog.info "Command [%s] => [%d]" cmd rsp;
    rsp

let build_file total seconds command percfile outfile = 
    let rec aux count cmd = 
        if count <= 0 then
            cmd^" "^outfile
        else
            aux (count - seconds) (cmd^" "^percfile)
    in
    let cmd = aux total command in
    PLog.debug "Command [%s]" cmd;
    let rsp = Sys.command cmd in
    PLog.info "Command [%s] => [%d]" cmd rsp;
    rsp


