open Unix
open Printf
open Stdlib

open Tools
open Log
open Env

module Command : ELT with type elt = string
module DurCommand : ELT with type elt = string
module PlayCommand : ELT with type elt = string
module PercFile : FILE_ELT
module OutFile : FILE_ELT
module Seconds : ELT with type elt = int
module Iterator : ELT with type elt = int
module LogLevel : ELT with type elt = level
module LogFile : FILE_ELT
module PlayResult : ELT with type elt = bool

val file_duration : string -> string -> int
val play_file : string -> string -> int
val build_file : int -> int -> string -> file -> file -> int 


