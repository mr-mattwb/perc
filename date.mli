open Unix
open Printf
open Stdlib
module Rxp = Str

type t = private int

val of_ints : int -> int -> int -> t
val of_strs : string -> string -> string -> t

val year0 : t

val now : unit -> t

module Sep : EnvParam.REX_ELT

val to_string : t -> string
val of_string : string -> t

val to_int : t -> int
val of_int : int -> t

val date_pat : string
val parse_date : string -> t

module Ser : Ser.ELT with type elt = t

module type ENV_PARAM = 
    sig
        val name : string
        val switches : string list
        val desc : string
    end
module type DEF_PARAM = 
    sig
        include ENV_PARAM
        val default : unit -> t
    end
module MakeEnv(P : DEF_PARAM) : EnvParam.ELT with type elt = t
module Now(E : ENV_PARAM) : EnvParam.ELT with type elt = t
