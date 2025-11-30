open Unix
open Printf
open Stdlib

open PLogLex

exception ParseFail of string

val input_entry : in_channel -> data entry option
val input_channel : in_channel -> data entry list
val input_file : Tools.file -> data entry list

module Ids : Set.S with type elt = id
val get_ids : 'a entry list ->  id list

val get_call : id -> 'a entry list -> 'a entry call
val calls_of_entries : 'a entry list -> 'a entry call list

val input_calls : Tools.file -> data entry call list


