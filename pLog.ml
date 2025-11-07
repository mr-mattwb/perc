open Unix
open Printf
open Stdlib

open Tools

let pfx_date = "(\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d)"
let pfx_time = "(\\d\\d:\\d\\d:\\d\\d)"
let pfx_msec = "(\\d\\d\\d)"
let pfx_callid = "([0-9A-F]*)"
let pfx_level = "([\\_0-9A-Z\\.-]+)"
let pfx_meth = "([a-zA-Z0-9\\.\\_]+)"
let calldata = "(.*)"
let msgfmt = pfx_date ^ "T" ^ pfx_time ^ "," ^ pfx_msec ^ "\\|" ^ pfx_callid ^ "\\|" ^ pfx_level ^ "\\|" ^ pfx_meth ^ "\\|" ^ calldata

let datefmt = "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)"
let timefmt = "(\\d\\d):(\\d\\d):(\\d\\d)"
let msecfmt = "(\\d\\d\\d)"
let idfmt = "([A-F0-9]+)"
let levelfmt = "([A-Z0-9\\.]+)-([A-Z]+)"
let methfmt = "([a-zA-Z0-9\\.\\_]+)"

let line = "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true"

let chainline = "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<"

let chain_pat = "chaining from >(.+)< to >(.*)<"

type id = string

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : int;
    id : id;
    ivr : string;
    level : string;
    meth : string;
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

let parse_msec ms = int_of_string ms
let parse_ivr twoitems = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:levelfmt twoitems) in
    ss.(1), ss.(2)
let parse_link item = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:chain_pat item) in
    ss.(1), ss.(2)

let entry_match line = Pcre.pmatch ~pat:("^"^msgfmt^"$") line
let parse_entry line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:msgfmt line) in
    let ivr,level = parse_ivr ss.(5) in
    { date = Date.parse_date ss.(1);
      time = Time.parse_time ss.(2);
      msec = parse_msec ss.(3);
      id = ss.(4);
      ivr = ivr;
      level = level;
      meth = ss.(6);
      data = ss.(7)
    }
let match_entry line = Pcre.pmatch ~pat:("^"^msgfmt^"$") line
let match_link e = Pcre.pmatch ~pat:chain_pat e.data
let match_id id e = id = e.id

let rec input_entry fin =
    match input_line fin with
    | None -> None
    | Some line when match_entry line -> 
        let e = parse_entry line in
        if e.id = "" then input_entry fin  
        else Some e
    | Some _ -> input_entry fin

let rec input_channel fin entries = 
    match input_entry fin with
    | None -> List.rev entries
    | Some e -> input_channel fin (e :: entries)
let input_file fname = Tools.with_in_file (fun fin -> (input_channel fin [])) fname

module Ids = Set.Make(String) 

let get_call (id : id) (entries : string entry list) : string entry call = 
    let aux acc e =
        if e.id = id then (e :: acc)
        else acc
    in
    let nodes = List.fold_left aux [] entries in
    { call_id = id;
      call_nodes = List.rev nodes
    }

let get_ids entries : id list =
    let aux acc e = Ids.add e.id acc in 
    let ids = List.fold_left aux Ids.empty entries in
    Ids.elements ids
let get_calls (entries : string entry list) : string entry call list = 
    let aux acc id = (get_call id entries) :: acc in
    List.fold_left aux [] (get_ids entries)

let parse_link (e : string entry) : link entry =
    let ss = Pcre.get_substrings (Pcre.exec ~pat:chain_pat e.data) in
    let link = { link_from = ss.(1); link_to = ss.(2) } in
    { e with data = link }

let get_call_links (call : string entry call) : link entry call =
    let aux acc e = 
        if match_link e then (parse_link e) :: acc
        else acc
    in
    { call_id = call.call_id;
      call_nodes = List.fold_left aux [] call.call_nodes
    }

let get_links (calls : string entry call list) : link entry call list = List.map get_call_links calls

let first_link (call : link entry call) : link entry = 
    let rec aux low = function
        | [] -> low
        | hd :: rest when hd.data.link_from < low.data.link_from -> aux hd rest
        | _ :: rest -> aux low rest
    in
    aux (List.hd call.call_nodes) (List.tl call.call_nodes)

let next_link (last : link entry) (call : link entry call) : link entry option =
    let rec aux = function
        | [] -> None
        | hd :: rest when last.data.link_to = hd.data.link_from -> Some hd
        | nxt :: rest -> aux rest
    in
    aux call.call_nodes

let call_links (fname : Tools.file) : link entry call list = 
    let entries = input_file fname in
    let calls = get_calls entries in
    get_links calls

let follow_path (call : link entry call) : link entry list = 
    let rec aux path node = 
        match next_link node call with
        | None -> List.rev (node :: path)
        | Some n -> aux (node :: path) n
    in
    let first = first_link call in
    aux [] first 

let rec inspect_path path = 
    let rec aux names = function
        | [] -> List.rev names
        | e :: es -> aux (e.data.link_from :: names) es
    in aux [] path

