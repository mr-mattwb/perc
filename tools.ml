open Unix
open Printf
open Stdlib

type file = string

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
let with_in_process use_in cmd = 
    use open_process_in close_in use_in cmd







