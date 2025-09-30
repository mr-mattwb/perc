open Unix
open Printf
open Stdlib
module Rxp = Str

open Tools

let pfx_date = "\\([0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\)"
let pfx_time = "T\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\),\\([0-9][0-9][0-9]\\)"
let pfx_callid = "|\\([0-9A-F]*\\)"
let pfx_ivr = "|\\([\\_0-9A-Z\\.-]+\\)"
let prefix = pfx_date ^ pfx_time ^ pfx_callid ^ pfx_ivr

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : int;
    id : string;
    ivr : string;
    data : 'a
}
type trans = string * string



let chain_str = prefix ^ "|reporting\\.CDRUtil|chaining from >\\(.+\\)< to >\\(.*\\)<"
let chain = Rxp.regexp chain_str
let fieldsep = Rxp.regexp ";"

let line = "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true"

let chainline = "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<"

let construct ds ts ms ids ivrs frs tos = {
    date = Date.of_string ds;
    time = Time.of_string ts;
    msec = int_of_string ms;
    id = ids;
    ivr = ivrs;
    data = (frs, tos)
}

let extract line =
    let fields = (Rxp.global_replace chain "\\1;\\2;\\3;\\4;\\5;\\6;\\7" line) in
    match Rxp.split fieldsep fields with
    | date :: time :: msec :: id :: ivr :: node_from :: node_to :: _ -> 
        construct date time msec id ivr node_from node_to 
    | _ -> 
        raise (Failure ("Fields ["^fields^"]"))

let rec chaining fin ls = 
    match input_line fin with
    | None -> List.rev ls
    | Some line when Rxp.string_match chain line 0 ->
        chaining fin (extract line :: ls)
    | Some line ->
        chaining fin ls
        
module CallMap = Map.Make(String)
module DateTimeMap = Map.Make(
    struct
        type t = Date.t * Time.t * int * string
        let compare = Stdlib.compare 
    end)
let accum cmap link = 
    match CallMap.find_opt link.id cmap with
    | None ->
        let dtmap = DateTimeMap.add (link.date, link.time, link.msec, fst link.data) link.data DateTimeMap.empty in
        CallMap.add link.id dtmap cmap
    | Some dtmap ->
        let dtmap' = DateTimeMap.add (link.date, link.time, link.msec, fst link.data) link.data dtmap in
        CallMap.add link.id dtmap' cmap

let load_calls fin = List.fold_left accum CallMap.empty (chaining fin [])

let rec call_nodes calls = List.map translate (CallMap.bindings calls)
and translate (id, nodes) = id, (List.rev (List.fold_left node_xlate [] (DateTimeMap.bindings nodes)))
and node_xlate acc ((ymd, hms, msc, _), (nf, nt)) = 
    match List.length acc with
    | n when n >= 1 ->
        begin
            match List.hd acc with 
            | n when n = nf -> nt :: acc
            | n -> nt :: nf :: acc
        end
    | _ -> 
        nt :: nf :: acc

let write_call fout (id, nodes) =
    fprintf fout "%s : " id;
    List.iter (fun n -> fprintf fout "%s " n) nodes;
    fprintf fout "\n%!"

let write_calls nodes fout = List.iter (write_call fout) nodes

let rec list_calls infile outfile = 
    let calls = with_in_file load_calls infile in
    let nodes = call_nodes calls in
    with_out_file (write_calls nodes) outfile


