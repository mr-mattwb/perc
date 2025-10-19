open Unix
open Printf
open Stdlib

type t = private int

val midnight : t

val of_ints : int -> int -> int -> t

module Sep : EnvParam.STR_ELT

val parse : string -> t

val to_string : t -> string
val of_string : string -> t

val to_int : t -> int
val of_int : int -> t

val now : unit -> t

val day : t

module Ser : Ser.ELT with type elt = t

module type ENV = EnvParam.ELT with type elt = t

module type ENV_PARAM = EnvParam.PARAMS
module type DEF_PARAM = EnvParam.DEFUNPARAMS with type elt = t

module MakeEnv(P : DEF_PARAM) : ENV
module Midnight(P  : ENV_PARAM) : ENV
module Now(P : ENV_PARAM) : ENV
