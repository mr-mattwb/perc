{
open Unix
open Printf
open Stdlib

open IniBase
open IniParse
}
let wsp = [' ' '\t' '\r']
let notCtxEqNl = [^ '[' '=' '\n' ' ' '\t' '\r' ';']
let openCtx = '['
let closeCtx = ']'
let comment = ';' 
let equal = '='
let eoln = '\n'
let notEq = [^ '=' ';' ]
let notWsp = [^ ';' ' ' '\t' '\r']
let notEqWsp = [^ ';' '=' ' ' '\t' '\r']
let notWspSemi = [^ ';' ' ' '\t' '\r' '\n']
let notEof = [^ ';' '\n' ]
let wspcs = [' ' '\t']
let notSpc = [^ ' ' '\t']

rule ini = parse
    wsp*                                            { ini lexbuf }
|   comment [^'\n']* (eoln|eof)                     { ini lexbuf }
|   eoln                                            { ini lexbuf }
|   eof                                             { EOF }
|   (notCtxEqNl notEq+ notEqWsp as key)             { KEY key }
|   equal wspcs* (notSpc notEof* notWspSemi as r)   { RESULT r }
|   openCtx ([^ ']']+ as ctx) closeCtx              { CONTEXT ctx }



