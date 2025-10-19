open Unix
open Printf
open Stdlib

open Tools


module type PARAMS =
    sig
        val path : unit -> file
    end

module type ELT =
    sig
        include PARAMS
        val get_file : unit -> string
        val put_file : string -> unit
        val exists : unit -> bool
        val file : unit -> file option
        val base : unit -> file
        val dir : unit -> file
        val is_dir : unit -> bool
        val touch : unit -> unit
        val mkdir : ?perms:int -> unit -> unit
    end

module Make(P : PARAMS) : ELT


