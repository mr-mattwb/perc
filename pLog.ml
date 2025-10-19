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

type id = string
type 'a entry = {
    time : DateTime.t;
    msec : int;
    id : id;
    ivr : string;
    data : 'a
}
type node = string
type link = node * node

let chain_str = prefix ^ "|reporting\\.CDRUtil|chaining from >\\(.+\\)< to >\\(.*\\)<"
let chain = Rxp.regexp chain_str
let fieldsep = Rxp.regexp ";"

let line = "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true"

let chainline = "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<"

let construct ds ts ms ids ivrs frs tos = {
    time = DateTime.of_strings ds ts;
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

let chain_file fname = Tools.with_in_file (fun fin -> chaining fin []) fname 

let entry_hash a = 
    let thash = (DateTime.hash a.time) * 1000 in
    thash + a.msec

module CallMap = Map.Make(String)
module DateTimeSet = Set.Make(
    struct
        type t = (string * string) entry 
        let compare a b =
            match (entry_hash a) - (entry_hash b) with
            | n when n <> 0 -> n
            | _ -> String.compare (fst a.data) (fst b.data) 

    end)

type node_set = DateTimeSet.t
type calls = node_set CallMap.t

let accum cmap link = 
    match CallMap.find_opt link.id cmap with
    | None ->
        let dtset = DateTimeSet.add link DateTimeSet.empty in
        CallMap.add link.id dtset cmap
    | Some dtset ->
        let dtset = DateTimeSet.add link dtset in
        CallMap.add link.id dtset cmap
let load_calls fin = List.fold_left accum CallMap.empty (chaining fin [])
let load_file fname = Tools.with_in_file load_calls fname

let rec call_nodes calls = List.map translate (CallMap.bindings calls)
and translate (id, nodes) = id, (List.rev (List.fold_left node_xlate [] (DateTimeSet.elements nodes)))
and node_xlate acc link = 
    match List.length acc with
    | n when n >= 1 ->
        begin
            match List.hd acc with 
            | n when n = (fst link.data) -> (snd link.data) :: acc
            | n -> (snd link.data) :: (fst link.data) :: acc
        end
    | _ -> 
        (snd link.data) :: (fst link.data) :: acc



