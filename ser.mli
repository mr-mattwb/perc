open Unix
open Printf
open Stdlib

open Tools

module type ELT =
    sig
        type elt
        val of_string : string -> elt
        val to_string : elt -> string
    end

module Str : ELT with type elt = string
module Int : ELT with type elt = int
module Flt : ELT with type elt = float
module Bool : ELT with type elt = bool

module List(E : ELT) : ELT with type elt = E.elt list


