open Unix
open Printf
open Stdlib

type t =
    | Eof
    | Context of string 
    | Pair of string * string

let putenv ?ctx key result =
    let k = 
        match ctx with
        | None -> key
        | Some ctx -> ctx^"."^key
    in Unix.putenv k result

let rec parse_env ?ctx main lex = 
    match main lex with
    | Eof -> ()
    | Context c -> parse_env ~ctx:c main lex
    | Pair (k, v) ->
        let c, k' = 
            match ctx with
            | None -> "", k
            | Some c -> c, c^"."^k
        in
        Unix.putenv k' v; 
        parse_env ~ctx:c main lex


