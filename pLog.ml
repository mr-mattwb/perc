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

let chain_pat = "reporting\\.CDRUtil|chaining from >(.+)< to >(.*)<"

type ms = int
type id = string

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : ms;
    id : id;
    ivr : string;
    level : string;
    meth : string;
    data : 'a
}

let parse_msec ms = int_of_string ms
let parse_ivr twoitems = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:levelfmt twoitems) in
    ss.(1), ss.(2)
let parse_link item = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:chain_pat item) in
    ss.(1), ss.(2)

let entry_match line = Pcre.pmatch ~pat:msgfmt line
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
let match_entry line = Pcre.pmatch ~pat:msgfmt line
let match_link e = Pcre.pmatch ~pat:chain_pat e.data
let match_id id e = id = e.id

let rec input_entry fin = 
    match Tools.input_line fin with
    | Some line when match_entry line -> Some (parse_entry line)
    | Some _ -> input_entry fin
    | None -> None

let rec process_channel acc fin = 
    match input_entry fin with
    | None -> List.rev acc
    | Some e -> process_channel (e :: acc) fin

type entries = string entry list 
type ids = id list
type call = string entry list
type chain = (string * string) entry list

let input_file fname : entries = Tools.with_in_file (process_channel []) fname

let filter_links ls = List.filter match_link ls

module IdSet = Set.Make(
    struct
        type t = string 
        let compare = compare
    end)
let call_ids entries : ids = 
    let aux acc e = IdSet.add e.id acc in
    IdSet.elements (List.fold_left aux IdSet.empty entries)


let link e = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:chain_pat e.data) in
    ss.(1), ss.(2)
let link_from e = fst (link e)
let link_to e = snd (link e)
k
let call_nodes id entries : call = List.filter (match_id id) entries
let filter_links nodes : chain = 
    let aux acc e =
        if match_link e then
            ({ e with data = link e } :: acc)
        else
            acc
    in
    List.fold_left aux [] nodes

