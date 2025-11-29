open Unix
open Printf
open Stdlib

type id = string

type priority = 
    | INFO
    | DEBUG
    | WARN
    | ERROR

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : int;
    id : id;
    ivr : string;
    prio : priority;
    func : string;
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
type state = {
    state : string
}
type data = 
    | Link of link
    | Label of label
    | State of state
    | Other of string

val of_strs : (string * string * string) -> (string * string * string) -> 
    string -> string -> string -> string -> string -> 'a -> 'a entry

val entry_pat : string
val link_pat : string
val label_pat : string
val state_pat : string
val data_pat : string

val entry_match : string -> bool
val link_match : string -> bool
val label_match : string -> bool
val state_match : string -> bool

module PriSer : Ser.ELT with type elt = priority

