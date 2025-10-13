{
open Unix
open Printf
open Stdlib

open PropBase
open IniParse
}
let wsp = [' ' '\t' '\r']
let comment = ';' 
let eoln = '\n'
let keyStart = [^ '[' '=' '\n' ' ' '\t' '\r' ';']
let keyContents = [^ '=' ';' ]
let keyEnd = [^ ';' '=' ' ' '\t' '\r']
let equal = '='
let wspcs = [' ' '\t']
let openCtx = '['
let closeCtx = ']'
let resStart = [^ ' ' '\t']
let resCont = [^ ';' '\n' ]
let resEnd = [^ ';' ' ' '\t' '\r' '\n']
let notWsp = [^ ';' ' ' '\t' '\r']

rule main = parse
    wsp*                                            { main lexbuf }
|   comment [^'\n']* (eoln|eof)                     { main lexbuf }
|   eoln                                            { main lexbuf }
|   eof                                             { EOF }
|   (keyStart (keyContents+ keyEnd)* as key)        { KEY key }
|   equal wspcs*                                    { result lexbuf }
|   openCtx ([^ ']']* as ctx) closeCtx              { CONTEXT ctx }

and result = parse
| '"' (([^ '"']|"\\\"")* as r) '"'                  { RESULT r }
| (resStart (resCont* resEnd)*) as r                { RESULT r }



