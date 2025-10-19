open Unix
open Printf
open Stdlib
module Rxp = Str

open Tools

val prefix : string

type id = string
type 'a entry = {
    time : DateTime.t;
    msec : int;
    id : id;
    ivr : string;
    data : 'a
}
type node = string
type link = node * node

val chain_str : string
val chain : Rxp.regexp
val fieldsep : Rxp.regexp

val construct : string -> string -> string -> string -> string -> node -> node -> link entry

val extract : string -> link entry
val chaining : in_channel -> link entry list -> link entry list
val chain_file : Tools.file -> link entry list

val entry_hash : 'a entry -> int

module CallMap : Map.S with type key = string
module DateTimeSet : Set.S with type elt = link entry

type node_set = DateTimeSet.t
type calls =  node_set CallMap.t

val accum : calls -> DateTimeSet.elt -> calls
val load_calls : in_channel -> calls
val load_file : Tools.file -> calls
val call_nodes : calls -> (id * node list) list
val translate : id * node_set -> id * node list
val node_xlate : node list -> link entry -> node list

