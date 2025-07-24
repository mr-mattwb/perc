{
open Unix
open Printf
open Stdlib
}
let wsp = [' ' '\t' '\n' '\r']
let openCtx = '['
let closeCtx = ']'
let comment = ';' 
let notEq = [^ '=' ';' ]
let notWsp = [^ ';' ' ' '\t' '\n' '\r']
let notEqWsp = [^ ';' '=' ' ' '\t' '\n' '\r']
let notWspSemi = [^ ';' ' ' '\t' '\n' '\r']
let notEof = [^ ';' ]

rule ini = parse
    wsp*                                    { ini lexbuf }
|   openCtx ([^ ']']+ as ctx) closeCtx      { `Context ctx }
|   (notEq+ notEqWsp as key) wsp*           { equal key lexbuf }
|   comment _* eof                          { `Pair ("", "") }

and equal key = parse
| eof                                       { `Pair (key, "") }
| comment _* eof                            { `Pair (key, "") }
| wsp+                                      { equal key lexbuf }
| '=' wsp*                                  { rest key lexbuf }

and rest key = parse
| notEof* notWspSemi  as r                  { `Pair (key, r)  }
| eof                                       { `Pair (key, "") }
| comment _* eof                            { `Pair (key, "") }

