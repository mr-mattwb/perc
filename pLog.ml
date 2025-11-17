
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
let labelline = "2025-08-25T05:28:20,289|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: dummy_dm"
let labelline2 = "2025-08-25T05:24:35,164|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-INFO |decision.languageSwitchOB|Exiting method. Label: Switch_English_OB"

let data_fmt = "(.*)"
let chain_entry_fmt = "chaining from >(.+)< to >(.*)<"
let label_entry_fmt = "^Label: (.*)$"

let entry_pat = datefmt ^ "T" ^ timefmt ^ "," ^ msecfmt ^ "\\|" ^ callidfmt ^ "\\|" ^ priofmt ^ "[ ]*\\|" ^ procfmt ^ "\\|"
let data_pat = entry_pat ^ data_fmt
let chain_pat = entry_pat ^ chain_entry_fmt
let label_pat = entry_pat ^ label_entry_fmt

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
type label = {
    label : string
}

type data = 
    | Chain of link entry
    | Label of label entry

let entry_match line = Pcre.pmatch ~pat:data_pat line
let link_match line = Pcre.pmatch ~pat:chain_pat line
let label_match line = Pcre.pmatch ~pat:label_pat line

let link_entry_match e = Pcre.pmatch ~pat:chain_entry_fmt e.data

let parse_link line =
    let ss = Pcre.get_substrings (Pcre.exec ~pat:chain_entry_fmt line) in
    { link_from=ss.(1); link_to=ss.(2) }

let parse_link_entry e = parse_link e.data

let parse_entry line =
    let ss = Pcre.get_substrings (Pcre.exec ~pat:data_pat line) in
    { date = Date.of_strs ss.(1) ss.(2) ss.(3);
      time = Time.of_strs ss.(4) ss.(5) ss.(6);
      msec = int_of_string ss.(7);
      callid = ss.(8);
      ivr = ss.(9);
      prio = ss.(10);
      proc = ss.(11);
      data = ss.(12)
    }

let rec input_entry fin = 
    match Tools.input_line fin with
    | None -> None
    | Some line when entry_match line -> Some (parse_entry line)
    | Some _ -> input_entry fin

let input_channel fin = 
    let rec aux data = 
        match input_entry fin with
        | None -> data
        | Some e -> aux (e :: data)
    in aux []

let input_file fname = Tools.with_in_file input_channel fname

let link_of_entry e = { e with data = parse_link_entry e }
let links_of_entries = 
    let rec aux links = function
        | [] -> links
        | n :: ns when link_entry_match n -> aux ((link_of_entry n) :: links) ns
        | _ :: ns -> aux links ns
    in aux [] 

module Ids = Set.Make(String)
let get_ids entries = 
    let aux acc e = Ids.add e.callid acc in
    Ids.elements (List.fold_left aux Ids.empty entries)

let get_call id entries = 
    let rec aux ls = function
        | [] -> {call_id = id; call_nodes = ls}
        | n :: ns when n.callid = id -> aux (n :: ls) ns
        | _ :: ns -> aux ls ns
    in aux [] entries

let get_calls entries = 
    let aux acc id = (get_call id entries) :: acc in 
    List.fold_left aux [] (get_ids entries)

let calls_of_links links = get_calls links
let calls_of_entries entries = calls_of_links (links_of_entries entries)
let input_calls fname = calls_of_entries (input_file fname)



