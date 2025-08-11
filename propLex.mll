{
open Unix
open Printf
open Stdlib
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

rule prop = parse
| wspc*                                                 { prop lexbuf }
| comment  nEoln*                                       { prop lexbuf } 
| eoln                                                  { prop lexbuf }
| equal extracmnt                                       { prop lexbuf }
| eof                                                   { `EOF }
| (keyStart (keyMid* keyEnd)*) as key                   { `KEY key }
| equal ((identStart (identMid* identEnd)*) as ident)   { `IDENT ident }











