open Unix
open Printf
open Stdlib

open Tools

type context = string
type key = string
type pair = key * string

type elt = {
    context : context;
    pairs : pair list
}

type t =
    | Eof
    | Context of context 
    | Pair of pair

val putenv : ?ctx:string -> string -> string -> unit
val parse : (Lexing.lexbuf -> t) -> Lexing.lexbuf -> t
val load_env : ?ctx:string -> (Lexing.lexbuf -> t) -> Lexing.lexbuf -> unit
val with_file_env : (Lexing.lexbuf -> t) -> file -> unit
val load_ctx : (Lexing.lexbuf -> t) -> Lexing.lexbuf -> elt list

val to_buffer : Buffer.t -> elt list -> unit
val to_string : elt list -> string
val to_channel : out_channel -> elt list -> unit
val to_stdout : elt list -> unit
val to_stderr : elt list -> unit

