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
module Int32 : ELT with type elt = int32
module Int64 : ELT with type elt = int64
module Flt : ELT with type elt = float
module Bool : ELT with type elt = bool

module List(E : ELT) : ELT with type elt = E.elt list
module Option(E : ELT) : ELT with type elt = E.elt option
module type OPTION = 
    sig
        val none : string
    end
module MakeOption(E : ELT)(O : OPTION) : ELT with type elt = E.elt option

module Time : ELT with type elt = private int 

module Ucid : 
    sig
        include ELT with type elt = Tools.ucid
        val make : ?length:int -> unit -> elt
    end

