{
open Unix
open Printf
open Stdlib

let gSeparator = ';'

let unescape src =
    let slen = String.length src in
    let buf = Buffer.create slen in
    let rec aux = function
    | n when n >= slen -> Buffer.contents buf
    | n when src.[n] = '\\' && src.[n+1] = gSeparator -> 
        Buffer.add_char buf gSeparator; 
        aux (n+2)
    | n ->
        Buffer.add_char buf src.[n];
        aux (n+1)
    in
    match slen with
    | 0 -> ""
    | 1 -> src
    | n -> aux 0
}

let wspc = [ ' ' '\t' '\r' '\n' ]
let begItem = ([^ ';' ' ' '\t' '\r' '\n' ]|"\\;")
let midItem = ([^ ';' '\n' ]|"\\;")
let endItem = ([^ ';' ' ' '\t' '\r' '\n' ]|"\\;")
let eoln = '\n'

rule itemList esc = parse
| wspc+                                 { itemList esc lexbuf }
| (begItem midItem* endItem*) as r      { (unescape r) :: (itemList false lexbuf) }
| ';'                                   { 
    if esc then "" :: (itemList true lexbuf)
    else itemList true lexbuf 
    }
| (eoln|eof)                            { 
    if esc then [""]
    else [] 
    }


{
open Unix
open Stdlib

let item_list str = itemList true (Lexing.from_string str)

}




