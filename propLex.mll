{
open Unix
open Printf
open Stdlib

open PropParse

}

let comment = '#'
let eoln = '\n'
let wspc = [' ' '\t']
let nWspc = [^ ' ' '\t']
let nEoln = [^ '\n']
let equal = wspc* '=' wspc*

let keyMid = [^ '=' '#' '\n' ]
let keyTerm = [^ '=' '#' ' ' '\t' '\n' ]

let identMid = [^ '#' '\n' ]
let idTerm = [^ '#' ' ' '\t' '\n' ]

let extracmnt = comment nEoln*

rule main = parse
| wspc*                                                 { main lexbuf }
| comment  nEoln*                                       { main lexbuf } 
| eoln                                                  { main lexbuf }
| equal extracmnt                                       { main lexbuf }
| eof                                                   { EOF }
| (keyTerm (keyMid* keyTerm)*) as key                   { KEY key }
| equal                                                 { ident lexbuf }

and ident = parse
| '"' (([^ '"']|"\\\"")* as ident) '"'                  { IDENT ident }
| ((idTerm (identMid* idTerm)*) as ident)         { IDENT ident }











