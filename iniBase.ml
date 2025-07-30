open Unix
open Printf
open Stdlib

type context = string
type key = string
type pair = key * string

type elt = {
    context : context;
    pairs : pair list
}

type t =
    | Eof
    | Context of context
    | Pair of pair

let putenv ?ctx key result =
    let k = 
        match ctx with
        | None -> key
        | Some ctx -> ctx^"."^key
    in Unix.putenv k result

let parse main lex = main lex

let rec load_env ?ctx main lex = 
    match main lex with
    | Eof -> ()
    | Context c -> load_env ~ctx:c main lex
    | Pair (k, v) ->
        let c, k' = 
            match ctx with
            | None -> "", k
            | Some c -> c, c^"."^k
        in
        Unix.putenv k' v; 
        load_env ~ctx:c main lex

let load_ctx main lex = 
    let rec aux acc ctx pairs  = function
        | Eof -> acc
        | Context c -> 
            let item = { context = ctx; pairs = pairs } in
            aux (item :: acc) c [] (main lex) 
        | Pair p -> aux acc ctx (p :: pairs) (main lex)
    in
    match main lex with
    | Eof -> []
    | Context ctx -> aux [] ctx [] (main lex)
    | Pair p -> aux [] "" [p] (main lex)

let to_buffer buf ls = 
    let add_items (k, v) = bprintf buf "%s=%s\n" k v in
    let add item = 
        bprintf buf "\n[%s]\n" item.context;
        List.iter add_items item.pairs
    in
    List.iter add ls

let to_string ls = 
    let buf = Buffer.create 1024 in
    to_buffer buf ls;
    Buffer.contents buf

let to_channel fout ls = 
    output_string fout (to_string ls);
    flush fout

let to_stdout ls = to_channel stdout ls
let to_stderr ls = to_channel stderr ls

