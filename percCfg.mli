open Unix
open Printf
open Stdlib

open Tools
open Log
open Env

module BuildCommand : ELT with type elt = string
module DurCommand : ELT with type elt = string
module PlayCommand : ELT with type elt = string
module OutFile : FILE_ELT
module Seconds : ELT with type elt = int
module LogLevel : ELT with type elt = level
module Play : ELT with type elt = bool
module FileExt : ELT with type elt = string

val file_duration : string -> int
val play_file : unit -> int
val build_file : int -> file -> int 
val with_percolator_file : (file -> 'a) -> 'a

