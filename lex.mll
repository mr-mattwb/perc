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
    | '"' ([^'"']* as qu) '"'                   { qu }
    | [' ' '\t']* ['#' '\r' '\n'] _* eof        { "" }
    | ([^ '#' '\r' '\n']* as v)                 { v }

and commalist spc = parse
    | (([^ ',']|"\\,")* as token)    { 
            let item1 = token in
            let item2 = commalist false lexbuf in
            item1 :: item2
        }
    | ','      { 
            if spc then "" :: (commalist true lexbuf)
            else commalist true lexbuf 
        }
    | eof      { 
            if spc then "" :: []
            else []
        }

and commas spc = parse
    | '"' (([^ '"']|"\\\"")* as quoted) '"' { quoted :: (commas false lexbuf) }
    | (([^',' '"']|"\\,")* as fld)          { fld :: (commas false lexbuf) }
    | [' ' '\t']* ',' [' ' '\t']*           { 
        if spc then "" :: commas true lexbuf 
        else commas true lexbuf }
    | eof                                   { 
        if spc then "" :: [] 
        else [] }


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

let comma_list line = 
    List.map (fun fld -> String.trim fld) 
        (commas false (Lexing.from_string line))


}





