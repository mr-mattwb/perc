{
open Unix
open Printf
open Stdlib
}

let ws = [' ' '\n' '\r' '\t']
let keyitem = [^ ' ' '=' '#']
let spcs = [' ']
let comment = '#' _* eof
let equal = '=' spcs*

let wspc = [' ' '\t']
let not_term = [^ '#' ' ' '\r' '\n']

let quoted = '"' ([^'"']* as qu) '"'

rule keyvaluepair = parse
    | comment            { ("", "") }
    | (keyitem* spcs* keyitem)*  {
            let key = Lexing.lexeme lexbuf in
            (key, equalpart lexbuf) 
                            }
    | eof                   { 
            (Lexing.lexeme lexbuf, "") 
                            }

and equalpart = parse
    | comment                                   { "" }
    | spcs*                                     { equalpart lexbuf }
    | equal spcs*                               { valpart lexbuf }
    
and valpart = parse
    | quoted                                    { qu }
    | ((not_term|[' ' '\t']*not_term)* as v)    { v }

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
    | "'" (([^ '\'']|"\\'")* as quoted) "'" { quoted :: (commas false lexbuf) }
    | (([^',' '"' '\'']|"\\,")* as fld)          { fld :: (commas false lexbuf) }
    | [' ' '\t']* ',' [' ' '\t']*           { 
        if spc then "" :: commas true lexbuf 
        else commas true lexbuf }
    | eof                                   { 
        if spc then "" :: [] 
        else [] }

and cfree = parse
    | eof                               { `Eof }
    | [' ' '\t']+                       { cfree lexbuf }
    | (['0'-'9']+ as num)               { 
        let op = cfree lexbuf in
        let nxt = cfree lexbuf in
        `Expr (num, op, nxt)
        }
    | '+'                               { `Add }
    | '-'                               { `Sub }
    | "mul"                             { `Mul }
    | "div"                             { `Div }



{
open Unix
open Stdlib

let parse line = 
    let k, v = keyvaluepair (Lexing.from_string line) in
    k, v

let rec load_line line = 
    let k, v = parse line in
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
        (commas true (Lexing.from_string line))


}





