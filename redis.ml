open Unix
open Printf
open Stdlib

type redis

external connect : string -> int -> redis = "caml_redisConnect"

