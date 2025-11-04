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


type entries = string entry list
type node = string


module IdSet = Set.Make(
    struct
        type t = string 
        let compare = compare
    end)

module type CHAIN = 
    sig
        type call_t 
        type link_t
        type t
        val lfrom : link_t -> node
        val lto : link_t -> node
        val get_links : call_t -> t
        val first_link : t -> link_t
        val next_link : link_t -> t -> link_t option
        val follow : t -> t
        val path_names : t -> node list
        val to_call : string entry list -> call_t
    end
module type ENTRIES = 
    sig
        type t
        type call_t
        type chain_t 
        val input_file : Tools.file -> t
        val get_call : id -> t -> call_t
        val call_ids : t -> id list
        val get_calls : t -> (id * call_t) list
        val of_call : t -> string entry list
        val get_links : t -> (id * chain_t) list
        val input_chains : Tools.file -> (id * chain_t) list
        val follow : t -> (id * chain_t) list
        val input_paths : Tools.file -> (id * chain_t) list
    end

module Chain : CHAIN = 
    struct
        type call_t = string entry list
        type link_t = (node * node) entry
        type t = link_t list
        let lfrom (e : link_t) = fst e.data
        let lto (e : link_t) = snd e.data
        let link (e : string entry) : node * node = 
            let ss = Pcre.get_substrings (Pcre.exec ~pat:chain_pat e.data) in
            ss.(1), ss.(2)
        let get_links (call : call_t) : t = 
            let aux acc e =
                if match_link e then
                    ({ e with data = link e } :: acc)
                else
                    acc
            in
            List.fold_left aux [] call
        let first_link (c : t) : link_t =
            let rec aux lws = function
                | [] -> lws
                | nxt :: rst when fst (lws.data) < fst (nxt.data) -> aux lws rst
                | nxt :: rst -> aux nxt rst
            in 
            aux (List.hd c) (List.tl c)
        let next_link (ff : link_t) (links : t) : link_t option =
            let rec aux = function
                | [] -> None
                | nxt :: rst when (snd ff.data) = (fst nxt.data) -> Some nxt
                | _ :: rst -> aux rst
            in aux links
        let follow (links : t) : t = 
            let rec aux ff acc = 
                match next_link ff links with
                | None -> List.rev acc
                | Some n -> aux n (n :: acc)
            in
            match links with
            | [] -> []
            | lnk ->
                let start = first_link lnk in
                aux start (start :: [])
        let path_names (links : t) : node list = 
            let rec aux acc = function
                | [] -> List.rev acc
                | e :: es -> aux ((fst e.data) :: acc) es
            in aux [] links
        let to_call (e : string entry list) : call_t = e
    end

module Entries : ENTRIES with type chain_t = Chain.t =
    struct
        type t = string entry list
        type call_t  = Chain.call_t
        type chain_t = Chain.t
        let rec input_entry fin : string entry option = 
            match Tools.input_line fin with
            | Some line when match_entry line -> Some (parse_entry line)
            | Some _ -> input_entry fin
            | None -> None
        let rec process_channel acc fin = 
            match input_entry fin with
            | None -> List.rev acc
            | Some e -> process_channel (e :: acc) fin
        let input_file (fname : Tools.file) : t = Tools.with_in_file (process_channel []) fname
        let to_list (e : t) : string entry list = e 
        let get_call (id : id) (entries : t) : call_t = Chain.to_call (List.filter (match_id id) entries)
        let call_ids (entries : t) : id list = 
            let aux acc e = if e.id <> "" then IdSet.add e.id acc else acc in
            IdSet.elements (List.fold_left aux IdSet.empty entries)
        let get_calls (entries : t) : (id * call_t) list =
            let aux id = id, get_call id entries in
            let ids = call_ids entries in
            List.map aux ids
        let of_call c : string entry list = c
        let get_links (entries : t) =
            let aux (id, call) = id, Chain.get_links call in
            List.map aux (get_calls entries)
        let input_chains fname = get_links (input_file fname)
        let follow (entries : t) =
            let aux (id, links) = id, Chain.follow links in
            List.map aux (get_links entries)
        let input_paths fname = follow (input_file fname)
    end




