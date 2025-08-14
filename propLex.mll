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

let keyStart = [^ '=' '#' ' ' '\t' '\n' ]
let keyMid = [^ '=' '#' '\n' ]
let keyEnd = [^ '=' '#' ' ' '\t' '\n' ]

let identStart = [^ '#' ' ' '\t' '\n' ]
let identMid = [^ '#' '\n' ]
let identEnd = [^ '#' ' ' '\t' '\n' ]

let extracmnt = comment [^ 'n']*

rule main = parse
| wspc*                                                 { main lexbuf }
| comment  nEoln*                                       { main lexbuf } 
| eoln                                                  { main lexbuf }
| equal extracmnt                                       { main lexbuf }
| eof                                                   { EOF }
| (keyStart (keyMid* keyEnd)*) as key                   { KEY key }
| equal                                                 { ident lexbuf }

and ident = parse
| '"' (([^ '"']|"\\\"")* as ident) '"'                  { IDENT ident }
| ((identStart (identMid* identEnd)*) as ident)         { IDENT ident }











