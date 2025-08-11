{
open Unix
open Printf
open Stdlib

open PropBase
open IniParse
}
let wsp = [' ' '\t' '\r']
let keyStart = [^ '[' '=' '\n' ' ' '\t' '\r' ';']
let keyContents = [^ '=' ';' ]
let keyEnd = [^ ';' '=' ' ' '\t' '\r']
let resStart = [^ ' ' '\t']
let resCont = [^ ';' '\n' ]
let resEnd = [^ ';' ' ' '\t' '\r' '\n']
let openCtx = '['
let closeCtx = ']'
let comment = ';' 
let equal = '='
let eoln = '\n'
let notWsp = [^ ';' ' ' '\t' '\r']
let wspcs = [' ' '\t']

rule ini = parse
    wsp*                                            { ini lexbuf }
|   comment [^'\n']* (eoln|eof)                     { ini lexbuf }
|   eoln                                            { ini lexbuf }
|   eof                                             { EOF }
|   (keyStart keyContents+ keyEnd as key)             { KEY key }
|   equal wspcs* (resStart resCont* resEnd* as r)   { RESULT r }
|   openCtx ([^ ']']* as ctx) closeCtx              { CONTEXT ctx }



