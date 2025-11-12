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
    callid : id;
    ivr : string;
    prio : string;
    proc : string;
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

val link_match : string -> bool
val parse_link : string -> link entry
val input_link : in_channel -> link entry option
val input_channel : in_channel -> link entry list
val input_file : Tools.file -> link entry list

module Ids : Set.S with type elt = id
val get_ids : 'a entry list -> id list
val get_call : id -> 'a entry list -> 'a entry call
val get_calls : 'a entry list -> 'a entry call list

val find_first : link entry -> link entry list -> link entry
val find_next : link entry -> link entry list -> link entry option

val callpath : link entry call -> id list
