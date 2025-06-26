{
open Unix
open Printf
open Stdlib
}

let ws = [' ' '\n' '\r' '\t']

rule keyvaluepair = parse
    | '#' _* eof            { ("", "") }
    | ([^ ' ' '=' '#']*[' ']*[^ ' ' '=' '#'])*  {
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

and commalist = parse
    | ([^ ',']|"\\,")*      { 
            let item1 = Lexing.lexeme lexbuf in
            let item2 = commalist lexbuf in
            item1 :: item2
        }
    | ','                   { commalist lexbuf }

{
open Unix
open Stdlib

let rec load_line line = 
    let k, v = keyvaluepair (Lexing.from_string line) in
    Unix.putenv k v 

let rec load_channel fin = 
    match Tools.input_line fin with
    | None -> ()
    | Some line ->
        load_line line;
        load_channel fin

let load_file fname = Tools.with_in_file load_channel fname
}





