open Unix
open Printf
open Stdlib

type t =
    | Eof
    | Context of string 
    | Pair of string * string

val putenv : ?ctx:string -> string -> string -> unit
val parse_env : ?ctx:string -> (Lexing.lexbuf -> t) -> Lexing.lexbuf -> unit

