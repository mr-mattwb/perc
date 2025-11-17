open Unix
open Printf
open Stdlib

open Tools

val msgfmt : string

val data_fmt : string
val chain_entry_fmt : string
val label_entry_fmt : string

val entry_pat : string
val data_pat : string
val chain_pat : string
val label_pat : string

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
type label = {
    label : string
}
type data = 
    | Chain of link entry
    | Label of label entry

val entry_match : string -> bool
val link_match : string -> bool
val label_match : string -> bool
val link_entry_match : string entry -> bool

val parse_link : string -> link
val parse_entry : string -> string entry

val input_entry : in_channel -> string entry option
val input_channel : in_channel -> string entry list
val input_file : Tools.file -> string entry list

val link_of_entry : string entry -> link entry
val links_of_entries : string entry list -> link entry list

module Ids : Set.S with type elt = id
val get_ids : 'a entry list -> id list
val get_call : id -> 'a entry list -> 'a entry call
val get_calls : 'a entry list -> 'a entry call list
val calls_of_links : 'a entry list -> 'a entry call list
val calls_of_entries : string entry list -> link entry call list 
val input_calls : Tools.file -> link entry call list


