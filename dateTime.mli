open Unix
open Printf
open Stdlib
module Rxp = Str

type t = private (int * int)

val empty : t

val of_date_time : int -> int -> t
val make_date : int -> int -> int -> int
val make_time : int -> int -> int -> int
val make_date_time : int -> int -> int -> int -> int -> int -> t
val date_of_strs : string -> string -> string -> int
val time_of_strs : string -> string -> string -> int
val date_time_of_strs : string -> string -> string -> string -> string -> string -> t

module DateSep : EnvParam.REX_ELT
module TimeSep : EnvParam.REX_ELT
module DateTimeSep : EnvParam.REX_ELT

val year : t -> int
val month : t -> int
val day : t -> int
val hour : t -> int
val minute : t -> int
val second : t -> int

val to_string : t -> string
val of_string : string -> t
val of_strings : string -> string -> t

val now : unit -> t

module Ser : Ser.ELT 

module EnvMake(P : EnvParam.PARAMS) : EnvParam.ELT with type elt = t


