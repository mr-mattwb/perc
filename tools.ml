open Unix
open Printf
open Stdlib
module Rex = Str

type file = string
type dir = string
type cmd = string
type seconds = int
type return_code = int
type perms = int
type ucid = string

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
let fmt_in_process use_in fmt =
    let aux cmd = with_in_process use_in cmd in
    ksprintf aux fmt

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

let rec fold_channel fin fn init =
    let rec aux acc = 
        match input_line fin with
        | None -> acc
        | Some line -> 
            aux (fn line acc)
    in
    aux init
let map_channel fin fn =
    List.rev (fold_channel fin (fun line acc -> (fn line) :: acc) [])
let iter_channel fin fn = 
    fold_channel fin (fun line () -> fn line) ()

let fold_file fname fn init = 
    with_in_file (fun fin -> fold_channel fin fn init) fname 
let map_file fname fn =
    List.rev (fold_file fname (fun line acc -> (fn line) :: acc) [])
let iter_file fname fn = fold_file fname (fun line () -> fn line) ()

let get_file_as_list fname = map_file fname (fun x -> x)    

let with_temp_file pfx sfx fn =
    let fname = Filename.temp_file pfx sfx in
    try
        let rsp = fn fname in 
        (* Sys.remove fname; *)
        rsp
    with e ->
        (try Sys.remove fname with _ -> ());
        raise e


let spawn fn arg =
    let forkagain () = 
        Unix.dup2 Unix.stdin Unix.stdin;
        Unix.dup2 Unix.stdout Unix.stdout;
        Unix.dup2 Unix.stderr Unix.stderr;
        match Unix.fork () with
        | 0 ->  
            ignore(fn arg); 
            exit 0
        | pid -> 
            exit 0
    in
    match Unix.fork () with
    | 0 -> forkagain ()
    | pid -> Unix.waitpid [] (-1)

let int_of_string s = int_of_string (String.trim s)

let tolerint_of_string str = 
    let rex = Str.regexp {|^\([0-9]*\).*$|} in
    match Str.replace_first rex {|\1|} str with
    | "" -> 0
    | num -> 
        try Stdlib.int_of_string num
        with _ -> raise (Failure (sprintf "tolerint_of_string [%s]" num))

let ceil_of_string str =
    int_of_float (Float.ceil (float_of_string str))

let ucid_of_string s = s
let string_of_ucid s = s
let hexs = [| '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F' |]
let new_ucid ?(len=16) () = 
    let buf = Buffer.create len in
    let rec loop = function
        | n when n < len -> 
            Buffer.add_char buf hexs.(Random.int len);
            loop (n+1)
        
        | n ->
            Buffer.contents buf
    in loop 0

let make_intTable ?(len=16) () =
    let itbl = Hashtbl.create len in
    let rec loop = function
        | n when n < len ->
            Hashtbl.add itbl hexs.(n) n;
            loop (n+1)
        | _ ->
            itbl
    in 
    let loaded = loop 0 in
    Hashtbl.add loaded 'a' 10;
    Hashtbl.add loaded 'b' 11;
    Hashtbl.add loaded 'c' 12;
    Hashtbl.add loaded 'd' 13;
    Hashtbl.add loaded 'e' 14;
    Hashtbl.add loaded 'f' 15;
    loaded
let ints = make_intTable()

let int_of_ucid ucid =
    let rec loop iuc = function
        | n when n <= (String.length ucid) - 1 ->
            let nd = Hashtbl.find ints ucid.[n] in
            loop (nd + (iuc * 16)) (n + 1)
        | n -> 
            iuc
    in loop 0 0


