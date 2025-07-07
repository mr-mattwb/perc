open Unix
open Printf
open Stdlib

open Tools
open Log
open Env

module BuildCommand : STR_ELT
module DurCommand : STR_ELT 
module PlayCommand : STR_ELT
module OutFile : FILE_ELT
module Seconds : INT_ELT
module LogLevel : ELT with type elt = level
module Play : BOOL_ELT
module FileExt : STR_ELT

val file_duration : string -> int
val play_file : unit -> int
val build_file : int -> file -> int 
val with_percolator_file : (file -> 'a) -> 'a

