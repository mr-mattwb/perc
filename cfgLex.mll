{
open Unix
open Printf
open Stdlib
}

let ws = [' ' '\n' '\r' '\t']

rule keyvaluepair = parse
    | '#' _* eof            { ("", "") }
    | [^ ' ' '=' '#']*([' ']*[^ ' ' '=' '#'])*  {
            let key = Lexing.lexeme lexbuf in
            (key, equalpart lexbuf) 
                            }
    | eof                   { 
            (Lexing.lexeme lexbuf, "") 
                            }

and equalpart = parse
    | '#' _* eof     { "" }
    | [' ']*         { equalpart lexbuf }
    | '=' [' ']*     { valpart lexbuf }
and valpart = parse
    | [' ' '\t']* ['#' '\r' '\n'] _* eof        { "" }
    | ([^ '#' '\r' '\n']* as v)                 { v }


{
open Unix
open Stdlib

let input_line fin = 
    try Some (Stdlib.input_line fin)
    with End_of_file -> None

let rec load_line line = 
    let k, v = keyvaluepair (Lexing.from_string line) in
    Unix.putenv k v 

let rec load_channel fin = 
    match input_line fin with
    | None -> ()
    | Some line ->
        load_line line;
        load_channel fin

let rec load_file fname = with_in_file fname load_channel

and with_in_file fn fname = use open_in close_in fname fn
and use openf closef usef argf = 
    let ch = openf argf in
    try 
        usef ch;
        closef ch 
    with e ->
        (try closef ch with _ -> ());
        raise e

}





