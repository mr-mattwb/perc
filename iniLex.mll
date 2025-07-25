{
open Unix
open Printf
open Stdlib

open IniBase
open IniParse
}
let wsp = [' ' '\t' '\r']
let notCtxEqNl = [^ '[' '=' '\n' ' ' '\t' '\r']
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

rule ini = parse
    wsp*                                        { ini lexbuf }
|   comment _* eoln                             { ini lexbuf }
|   wsp* eoln                                   { ini lexbuf }
|   eof                                         { EOF }
|   (notCtxEqNl notEq+ notEqWsp as key) wsp*    { KEY key }
|   equal (notEof* notWspSemi as r)             { RESULT r }
|   openCtx ([^ ']']+ as ctx) closeCtx          { CONTEXT ctx }



