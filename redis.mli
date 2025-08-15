open Unix
open Printf
open Stdlib

type redis 

val connect : string -> int -> redis
