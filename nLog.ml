open Unix
open Printf
open Stdlib

type id = string

type priority = 
    | DEBUG
    | INFO
    | WARN
    | ERROR


type 'a entry = {
    entry_date : Date.t;
    entry_time : Time.t;
    entry_msec : int;
    entry_id   : string;
    entry_vers : string;
    entry_prio : priority;
    entry_func : string;
    entry_data : 'a
}

let year = "([1-2][0-9][0-9][0-9])"
let month = "(0[1-9]|1[0-2])"
let day = "(0[1-9]|[1-2][0-9]|3[0-1])"
let date = year^"-"^month^"-"^day

let hour = "([0-1][0-9]|2[0-3])"
let minute = "([0-5][0-9])"
let second = "([0-5][0-9])"
let time = hour^":"^minute^":"^second

let msec = "([0-9][0-9][0-9])"
let datetime = date^"T"^time^","^msec
let callid = "([A-F0-9]*)"
let version = "([A-Z0-9\\.\\-\\_]+)"
let priority = "([A-Z]+)"
let funcname = "([^ \  \n \t | ]*)"
let data = "([^\n]*)"
let logline = datetime^"\\|"^callid^"\\|"^version^"-"^priority^"[ ]*\\|"^funcname^"\\|"^data
let rxplogline = Pcre.regexp logline

let link_fmt = "chaining from >(.*)< to >(.*)<"
let label_fmt = "Label: (.*)"
let state_fmt = "(.*): entering state"
let link = Pcre.regexp link_fmt
let label = Pcre.regexp label_fmt
let state = Pcre.regexp state_fmt

let dataline = "chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<"
let linkline = "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<"
let labelline = "2025-08-25T05:28:20,289|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: dummy_dm"
let labelline2 = "2025-08-25T05:24:35,164|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-INFO |decision.languageSwitchOB|Exiting method. Label: Switch_English_OB"
let stateline = "2025-08-25T05:28:54,469|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state"

module PrioSer =
    struct
        type elt = priority
        let of_string s =
            match String.uppercase_ascii s with | "DEBUG" -> DEBUG | "INFO"  -> INFO
            | "WARN"  -> WARN
            | "ERROR" -> ERROR
            | unknown -> raise (Failure ("Invalid priority ["^s^"]"))
        let to_string = function
            | DEBUG   -> "DEBUG"
            | INFO    -> "INFO"
            | WARN    -> "WARN"
            | ERROR   -> "ERROR"
    end

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

let parse_link line = 
    let ss = Pcre.get_substrings (Pcre.exec ~rex:link line) in
    { link_from = ss.(1); link_to = ss.(2) }
let parse_label line = 
    let ss = Pcre.get_substrings (Pcre.exec ~rex:label line) in
    { label = ss.(1) }
let parse_state line = 
    let ss = Pcre.get_substrings (Pcre.exec ~rex:state line) in
    { state = ss.(1) }

let parse_data line = 
    match line with
    | _ when Pcre.pmatch ~rex:link line -> Link (parse_link line)
    | _ when Pcre.pmatch ~rex:label line -> Label (parse_label line)
    | _ when Pcre.pmatch ~rex:state line -> State (parse_state line)
    | _ -> Other line

let entry_matches line = Pcre.pmatch ~rex:rxplogline line

let parse_entry line =
    let ss = Pcre.get_substrings (Pcre.exec ~rex:rxplogline line) in {
        entry_date = Date.of_strs ss.(1) ss.(2) ss.(3);
        entry_time = Time.of_strs ss.(4) ss.(5) ss.(6);
        entry_msec = int_of_string ss.(7);
        entry_id   = ss.(8);
        entry_vers = ss.(9);
        entry_prio = PrioSer.of_string ss.(10);
        entry_func = ss.(11);
        entry_data = parse_data ss.(12)
    }

let rec read_entry fin line =
    let e = parse_entry line in
    match e.entry_data with
    | Other _ -> input_entry fin
    | _ -> Some e

and input_entry fin =
    match Tools.input_line fin with
    | None -> None
    | Some line when entry_matches line -> read_entry fin line
    | _ -> input_entry fin
    

let input_channel fin = 
    let rec aux path = 
        match input_entry fin with
        | None -> path
        | Some e -> aux (e :: path)
    in
    aux []

let input_file fname = Tools.with_in_file input_channel fname

type 'a call = {
    call_id : string;
    call_nodes : 'a list
}

module Id = Set.Make(String)
let get_ids entries = 
    let aux acc v = Id.add v.entry_id acc in
    Id.elements (List.fold_left aux Id.empty entries)
let get_call id entries = 
    let aux acc v = 
        if id = v.entry_id then v :: acc 
        else acc
        in {
            call_id = id;
            call_nodes = List.fold_left aux [] entries
        }
let calls_of_entries entries = 
    let aux acc id = (get_call id entries) :: acc in
    List.fold_left aux [] (get_ids entries)

let input_calls fname = calls_of_entries (input_file fname)

let link_nodes call = 
    let aux acc v = 
        match v.entry_data with
        | Link link -> link :: acc
        | _ -> acc
    in
    List.fold_left aux [] call.call_nodes

let first_link ?(link_from="Initialize") ?(link_to="welcid0100_Dummy_DM") links = 
    let find e = e.link_from = link_from && e.link_to = link_to in
    List.find find links

let next_link node links = 
    let rec aux = function
        | [] -> None
        | v :: vs when v.link_from = node.link_to -> Some v
        | _ :: vs -> aux vs
    in aux links
    
let call_path call = 
    let links = link_nodes call in 
    let rec aux curr path =
        match next_link curr links with
        | None -> List.rev path
        | Some nxt -> aux nxt (nxt :: path) 
    in
    let ff = first_link links in
    aux ff (ff :: [])

let call_id_path id entries = call_path (get_call id entries)

