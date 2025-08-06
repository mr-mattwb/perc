open Unix
open Printf
open Stdlib

open Tools
open Log
open Env

type return_code = int

module BuildCommand : STR_ELT
module DurCommand : STR_ELT
module PlayCommand : STR_ELT
module OutFile : FILE_ELT
module Seconds : INT_ELT
module Play : BOOL_ELT
module FileExt : STR_ELT

module LogLevel : Log.LEVEL_ENV
module LogTargets : Log.OUT_ENV
module LogName(N : Log.NAME) : Log.ELT
module Verbose : 
    sig
        include Env.BOOL_ELT
        val configure : unit -> unit
    end

module type ELT = 
    sig
        val file_duration : file -> return_code
        val play_file : unit -> return_code
        val build_file : seconds ->  file -> return_code
        val with_percolator_file : (file -> 'a) -> 'a
    end

include ELT

