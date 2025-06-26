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

module Str =
    struct
        type elt = string
        let of_string v = v
        let to_string v = v
    end
module Int = 
    struct
        type elt = int
        let of_string v = int_of_string (String.trim v)
        let to_string = string_of_int
    end
module Flt = 
    struct
        type elt = float
        let of_string v = float_of_string (String.trim v)
        let to_string = string_of_float
    end
module Bool = 
    struct
        type elt = bool
        let of_string = bool_of_string
        let to_string = string_of_bool
    end

module List(E : ELT) =
    struct
        type elt = E.elt list
    end
