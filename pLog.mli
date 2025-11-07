open Unix
open Printf
open Stdlib

open Tools

val msgfmt : string
val chain_pat : string

type id = string

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : int;
    id : id;
    ivr : string;
    level : string;
    meth : string;
    data : 'a
}
type 'a call = {
    call_id : id;
    call_nodes : 'a list
}
type link = {
    link_from : string;
    link_to : string
}

val parse_msec : string -> int
val parse_ivr : string -> string * string
val parse_link : string -> string * string

val entry_match : string -> bool
val parse_entry : string -> string entry
val match_entry : string -> bool
val match_link : string entry -> bool
val match_id : id -> string entry -> bool

val input_entry : in_channel -> string entry option
val input_channel : in_channel -> string entry list -> string entry list
val input_file : Tools.file -> string entry list

module Ids : Set.S with type elt = id

val get_call : id -> string entry list -> string entry call
val get_ids : string entry list -> id list
val get_calls : string entry list -> string entry call list

val parse_link : string entry -> link entry
val get_call_links : string entry call -> link entry call
val get_links : string entry call list -> link entry call list

val first_link : link entry call -> link entry
val next_link : link entry -> link entry call -> link entry option

val call_links : Tools.file -> link entry call list
val follow_path : link entry call -> link entry list
val inspect_path : link entry list -> string list



