open Unix
open Printf
open Stdlib

type id = string

type priority = 
    | DEBUG
    | INFO
    | WARN
    | ERROR


type 'a entry = {
    entry_date : Date.t;
    entry_time : Time.t;
    entry_msec : int;
    entry_id   : string;
    entry_vers : string;
    entry_prio : priority;
    entry_func : string;
    entry_data : 'a
}

val datetime : string
val logline : string
val rxplogline : Pcre.regexp

val link : Pcre.regexp
val label : Pcre.regexp
val state : Pcre.regexp

module PrioSer : Ser.ELT with type elt = priority

type link = {
    link_from : string;
    link_to : string
}
type label = {
    label : string
}
type state = {
    state : string
}
type data = 
    | Link of link
    | Label of label
    | State of state
    | Other of string

val parse_link : string -> link
val parse_label : string -> label
val parse_state : string -> state
val parse_data : string -> data

val entry_matches : string -> bool
val parse_entry : string -> data entry
val input_entry : in_channel -> data entry option

val input_channel : in_channel -> data entry list
val input_file : Tools.file -> data entry list

type 'a call = {
    call_id : string;
    call_nodes : 'a list
}

module Id : Set.S with type elt = id
val get_ids : data entry list -> string list
val get_call : id -> data entry list -> data entry call
val calls_of_entries : data entry list -> data entry call list
val input_calls : Tools.file -> data entry call list

val link_nodes : data entry call -> link list
val first_link : ?link_from:string -> ?link_to:string -> link list -> link
val next_link : link -> link list  -> link option
val call_path : data entry call -> link list

