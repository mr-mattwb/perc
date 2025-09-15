open Unix
open Printf
open Stdlib

open Tools
open EnvParam

module Soxi = Env.Cmd(
    struct
        let name = "SOXI"
        let switches = [ "--cmd-soxi" ]
        let desc = "soxi command"
        let default () = "/usr/bin/soxi"
    end)

let with_soxi sw fname = 
    fmt_in_process input_line "%s %s %s"
        (Soxi.get()) sw fname
let with_soxi_int sw fname = 
    match with_soxi sw fname with
    | None -> None
    | Some s -> Some (int_of_string s)
let with_soxi_float sw fname = 
    match with_soxi sw fname with
    | None -> None
    | Some s -> Some (float_of_string s)

let file_type fn = with_soxi "-t" fn
let sample_rate fn = with_soxi_int "-r" fn
let channels fn = with_soxi_int "-c" fn
let samples fn = with_soxi_int "-s" fn
let duration_hms fn = with_soxi "-d" fn
let duration fn = with_soxi_float "-D" fn
let bits_per_sample fn = with_soxi_int "-b" fn
let avg_bitrate fn = with_soxi "-B" fn
let sample_precision fn = with_soxi_int "-p" fn
let audio_encoding fn = with_soxi "-e" fn
let comments fn = with_soxi "-a" fn

