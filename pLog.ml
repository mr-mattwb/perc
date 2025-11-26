
type id = string

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : int;
    id : id;
    ivr : string;
    prio : string;
    func : string;
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
type state = {
    state : string
}
type data = 
    | Link of link
    | Label of label
    | State of state
    | Other of string

let datefmt = "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)"
let timefmt = "(\\d\\d):(\\d\\d):(\\d\\d)"
let msecfmt = "(\\d\\d\\d)"
let idfmt = "([A-F0-9]+)"
let priofmt = "([A-Z0-9\\.]+)-([A-Z]+)"
let funcfmt = "([a-zA-Z0-9\\.\\_]+)"
let datafmt = "(.*)"

let line = "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true"

let linkline = "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<"
let labelline = "2025-08-25T05:28:20,289|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: dummy_dm"
let labelline2 = "2025-08-25T05:24:35,164|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-INFO |decision.languageSwitchOB|Exiting method. Label: Switch_English_OB"
let stateline = "2025-08-25T05:28:54,469|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state"

let entryfmt = datefmt ^ "T" ^ timefmt ^ "," ^ msecfmt ^ "\\|" ^ idfmt ^ "\\|" ^ priofmt ^ "\\|" ^ funcfmt ^ "\\|" ^ datafmt

let data_fmt = "(.*)"
let link_fmt = "chaining from >(.+)< to >(.*)<"
let label_fmt = "Label: (.*)"
let state_fmt = "(.*): entering state"

let chain_entry_fmt = "^" ^ link_fmt ^ "$"
let label_entry_fmt = "^" ^ label_fmt ^ "$"
let state_entry_fmt = "^" ^ state_fmt ^ "$"

let entry_pat = datefmt ^ "T" ^ timefmt ^ "," ^ msecfmt ^ "\\|" ^ idfmt ^ "\\|" ^ priofmt ^ "[ ]*\\|" ^ funcfmt ^ "\\|"
let data_pat = entry_pat ^ data_fmt
let link_pat = entry_pat ^ link_fmt
let label_pat = entry_pat ^ label_fmt
let state_pat = entry_pat ^ state_fmt

let entry_match line = Pcre.pmatch ~pat:data_pat line
let link_match line = Pcre.pmatch ~pat:link_pat line
let label_match line = Pcre.pmatch ~pat:label_pat line
let state_match line = Pcre.pmatch ~pat:state_pat line

let parse_entry line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:data_pat line) in
    { date = Date.of_strs ss.(1) ss.(2) ss.(3);
      time = Time.of_strs ss.(4) ss.(5) ss.(6);
      msec = int_of_string ss.(7);
      id = ss.(8);
      ivr = ss.(9);
      prio = ss.(10);
      func = ss.(11);
      data = ss.(12) }

let parse_link line =
    let ss = Pcre.get_substrings (Pcre.exec ~pat:link_fmt line) in
    { link_from = ss.(1); link_to = ss.(2) }
let parse_label line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:label_fmt line) in
    { label = ss.(1) }
let parse_state line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:state_fmt line) in
    { state = ss.(1) }

let parse_link_entry line = 
    let entry = parse_entry line in
    { entry with data = Link (parse_link entry.data) }
let parse_label_entry line =
    let entry = parse_entry line in
    { entry with data = Label (parse_label entry.data) }
let parse_state_entry line = 
    let entry = parse_entry line in
    { entry with data = State (parse_state entry.data) }
let parse_other_entry line = 
    let entry = parse_entry line in
    { entry with data = Other entry.data }

let rec input_entry fin = 
    match Tools.input_line fin with
    | None -> None
    | Some line when link_match line -> Some (parse_link_entry line)
    | Some line when label_match line -> Some (parse_label_entry line)
    | Some line when state_match line -> Some (parse_state_entry line)
    | Some _ -> input_entry fin

let input_channel fin = 
    let rec aux path =
        match input_entry fin with
        | None -> List.rev path
        | Some x -> aux (x :: path) 
   in
   aux []

let input_file fname = Tools.with_in_file input_channel fname

module Id = Set.Make(String)
let get_ids entries = 
    let add set e = Id.add e.id set in
    Id.elements (List.fold_left add Id.empty entries)

let get_call id entries =
    let add acc e = 
        if id = e.id then e :: acc
        else acc
    in
    { call_id = id;
      call_nodes = (List.fold_left add [] entries) }

let calls_of_entries entries = 
    let aux acc id = (get_call id entries) :: acc in
    List.fold_left aux [] (get_ids entries)

let key node = ((Date.to_int node.date) * 1000000000) + ((Time.to_int node.time) * 1000) + node.msec

module NodeSet = Set.Make(
    struct
        type t = data entry
        let compare a b = 
            let ka = key a in
            let kb = key b in
            if ka = kb then Stdlib.compare a.data b.data
            else ka - kb 
    end)

