open Unix
open Printf
open Stdlib

open PLogLex

exception ParseFail of string

let rec input_entry fin = 
    match Tools.input_line fin with
    | None -> None
    | Some line when entry_match line -> parse_line fin line
    | Some _ -> input_entry fin 
and parse_line fin line =
    let e = PLogLex.entrydata (Lexing.from_string line) in
    match e.data with
    | Other _ -> input_entry fin
    | x -> Some e 

let input_channel fin = 
    let rec aux path = 
        match input_entry fin with
        | None -> path
        | Some e -> aux (e :: path) 
    in
    aux []

let input_file fname = Tools.with_in_file input_channel fname

module Ids = Set.Make(String)
let get_ids entries = 
    Ids.elements (List.fold_left (fun acc v -> Ids.add v.id acc) Ids.empty entries)
 
let get_call id entries = 
    let add acc v = 
        if v.id = id then v :: acc
        else acc
    in
    { call_id = id; 
      call_nodes = List.fold_left add [] entries }

let calls_of_entries entries =
    let add acc id = (get_call id entries) :: acc in
    List.fold_left add [] (get_ids entries)

let input_calls fname = calls_of_entries (input_file fname)

