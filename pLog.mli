open Unix
open Printf
open Stdlib
module Rxp = Str

open Tools

val prefix : string

type 'a entry {
    date : Date.t;
    time : Time.t;
    msec : int;
    id : string;
    ivr : string;
    data : 'a
}
type link = string * string

val chain_str : string
val chain : Rxp.regexp
val fieldsep : Rxp.regexp

val construct : string -> string -> string -> string -> string -> string -> string -> link entry

val extract : string -> link entry
val chaining : in_channel -> link entry list -> link entry list
val chain_file : Tools.file -> link entry list

val entry_hash : 'a entry -> int

module CallMap : 'a '
