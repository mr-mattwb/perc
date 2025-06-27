open Unix
open Printf
open Stdlib

type file = string
type seconds = int

let name = Sys.argv.(0)
let basename = Filename.basename name   

let input_line fin = 
    try Some (Stdlib.input_line fin)
    with End_of_file -> None

let use (openf : 'a -> 'b) (closef : 'b -> unit) (usef : 'b -> 'c) (argf : 'a) : 'c = 
    let ch = openf argf in
    try 
        let rc = usef ch in
        closef ch;
        rc
    with e ->
        (try closef ch with _ -> ());
        raise e

let with_in_file fn fname = use open_in close_in fn fname 
let with_out_file fn fname = use open_out close_out fn fname
let with_append_file fn fname = 
    let opener = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 in
    use opener close_out fn fname 
let with_in_process use_in cmd = 
    use open_process_in close_in use_in cmd

let file_size fname = (Unix.stat fname).st_size
let buffer_file fname = 
    let len = file_size fname in
    let buf = Buffer.create len in
    let get fin = Buffer.add_channel buf fin len; buf in
    with_in_file get fname
let get_file fname = Buffer.contents (buffer_file fname)
let put_file fname contents = 
    let write fout = 
        output_string fout contents;
        flush fout
    in
    with_out_file write fname
let append_file fname contents = 
    let write fout = 
        output_string fout contents;
        flush fout
    in
    with_append_file write fname

    
let getenv e = try Some (Unix.getenv e) with Not_found -> None



