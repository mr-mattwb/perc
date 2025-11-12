open Unix
open Printf
open Stdlib

open Tools

let pfx_date = "(\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d)"
let pfx_time = "(\\d\\d:\\d\\d:\\d\\d)"
let pfx_msec = "(\\d\\d\\d)"
let pfx_callid = "([0-9A-F]*)"
let pfx_prio = "([\\_0-9A-Z\\.-]+)"
let pfx_proc = "([a-zA-Z0-9\\.\\_]+)"
let calldata = "(.*)"
let msgfmt = pfx_date ^ "T" ^ pfx_time ^ "," ^ pfx_msec ^ "\\|" ^ pfx_callid ^ "\\|" ^ pfx_prio ^ "\\|" ^ pfx_proc ^ "\\|" ^ calldata

let datefmt = "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)"
let timefmt = "(\\d\\d):(\\d\\d):(\\d\\d)"
let msecfmt = "(\\d\\d\\d)"
let callidfmt = "([A-F0-9]+)"
let priofmt = "([A-Z0-9\\.]+)-([A-Z]+)"
let procfmt = "([a-zA-Z0-9\\.\\_]+)"

let line = "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true"

let chainline = "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<"

let chain_pat = "chaining from >(.+)< to >(.*)<"

let link_pat = datefmt ^ "T" ^ timefmt ^ "," ^ msecfmt ^ "\\|" ^ callidfmt ^ "\\|" ^ priofmt ^ "\\|" ^ procfmt ^ "\\|" ^ chain_pat

type id = string

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : int;
    callid : id;
    ivr : string;
    prio : string;
    proc : string;
    data : 'a
}
type 'a call = {
    call_id : id;
    call_nodes : 'a list
}
type link = {
    link_from : string;
    link_to : string
}

let link_match line = Pcre.pmatch ~pat:chain_pat line

let parse_link line =
    let ss = Pcre.get_substrings (Pcre.exec ~pat:link_pat line) in
    { date = Date.of_strs ss.(1) ss.(2) ss.(3);
      time = Time.of_strs ss.(4) ss.(5) ss.(6);
      msec = int_of_string ss.(7);
      callid = ss.(8);
      ivr = ss.(9);
      prio = ss.(10);
      proc = ss.(11);
      data = { link_from = ss.(12); link_to = ss.(13) }
    }

let rec input_link fin = 
    match input_line fin with
    | None -> None
    | Some line when link_match line -> Some (parse_link line)
    | Some _ -> input_link fin 

let input_channel fin = 
    let rec aux entries = 
        match input_link fin with
        | None -> List.rev entries
        | Some link -> aux (link :: entries)
    in aux []
let input_file fname = Tools.with_in_file input_channel fname

module Ids = Set.Make(String)
let get_ids links = 
    let rec aux set = function
        | [] -> Ids.elements set
        | link :: links -> aux (Ids.add link.callid set) links
    in aux Ids.empty links

let get_call id = 
    let rec aux nodes = function
        | [] -> { call_id = id; call_nodes = nodes }
        | n :: links when n.callid = id -> aux (n :: nodes) links
        | n :: links -> aux nodes links
    in aux [] 

let get_calls links = 
    let aux acc id = (get_call id links) :: acc in
    List.fold_left aux [] (get_ids links)

let rec find_first low = function
    | [] -> low
    | n :: ns when n.data.link_from < low.data.link_from -> find_first n ns
    | _ :: ns -> find_first low ns
let rec find_next curr = function
    | [] -> None
    | n :: ns when curr.data.link_to = n.data.link_from -> Some n
    | _ :: ns -> find_next curr ns

let callpath call = 
    let rec aux path curr = 
        match find_next curr call.call_nodes with
        | None -> List.rev path
        | Some n -> aux (n.data.link_from :: path) n 
    in 
    let ff = find_first (List.hd call.call_nodes) (List.tl call.call_nodes) in
    aux [ff.data.link_from] ff

