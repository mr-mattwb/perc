open Unix
open Printf
open Stdlib
module Rxp = Str

open Tools

let pfx_date = "\\([0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\)"
let pfx_time = "T\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\),[0-9][0-9][0-9]"
let pfx_callid = "|\\([0-9A-F]*\\)"
let pfx_ivr = "|\\([\\_0-9A-Z\\.-]+\\)"
let prefix = pfx_date ^ pfx_time ^ pfx_callid ^ pfx_ivr

type 'a entry = {
    date : Date.t;
    time : Time.t;
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

let construct ds ts ids ivrs frs tos = {
    date = Date.of_string ds;
    time = Time.of_string ts;
    id = ids;
    ivr = ivrs;
    data = (frs, tos)
}

let extract line =
    let fields = (Rxp.global_replace chain "\\1;\\2;\\3;\\4;\\5;\\6" line) in
    match Rxp.split fieldsep fields with
    | date :: time :: id :: ivr :: node_from :: node_to :: _ -> 
        construct date time id ivr node_from node_to 
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
        type t = Date.t * Time.t
        let compare = Stdlib.compare 
    end)
let accum cmap link = 
    match CallMap.find_opt link.id cmap with
    | None -> 
        let dtmap = DateTimeMap.add (link.date, link.time) link.data DateTimeMap.empty in
        CallMap.add link.id dtmap cmap
    | Some dtmap ->
        let cmap' = CallMap.remove link.id cmap in
        let dtmap' = DateTimeMap.add (link.date, link.time) link.data dtmap in
        CallMap.add link.id dtmap' cmap'

let load_calls fin = List.fold_left accum CallMap.empty (chaining fin [])



