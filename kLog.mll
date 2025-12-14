{
open Unix
open Printf
open Stdlib

type priority = 
    | DEBUG
    | INFO
    | WARN
    | ERROR

module PriSer =
    struct
        type elt = priority
        let of_string s = 
            match String.uppercase_ascii s with
            | "DEBUG" -> DEBUG
            | "INFO" -> INFO
            | "WARN" -> WARN
            | "ERROR" -> ERROR
            | _ -> raise (Failure ("Unknown priority ["^s^"]"))
        let to_string = function
            | DEBUG -> "DEBUG"
            | INFO -> "INFO"
            | WARN -> "WARN"
            | ERROR -> "ERROR"
    end
 
type id = string

type 'a entry = {
    entry_date : Date.t;
    entry_time : Time.t;
    entry_msec : int;
    entry_id   : id;
    entry_vers : string;
    entry_prio : priority;
    entry_func : string;
    entry_data : 'a
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

let pat = "([1-2][0-9][0-9][0-9])-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])"
let pat = pat ^ "T([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9]),[0-9][0-9][0-9]" (* date time msec *)
let pat = pat ^ "\\|([A-F0-9]+)"                                                 (* call-id *)
let pat = pat ^ "\\|([A-Z0-9.])+-(DEBUG|INFO|WARN|ERROR)[ ]*"                    (* Version-priority *)
let pat = pat ^ "\\|[^\\|]*"                                                     (* Function name *)
let entry_pat = pat ^ "\\|[^\n]*"                                                (* data *)
let entry_rex = Pcre.regexp entry_pat

}
let year = ['1'-'2']['0'-'9']['0'-'9']['0'-'9']
let month = ('0'['1'-'9']|'1'['0'-'2']) 
let day = ('0'['1'-'9']|['1'-'2']['0'-'9']|'3'['0'-'1'])
let hour = (['0'-'1']['0'-'9']|'2'['0'-'3'])
let minute = (['0'-'5']['0'-'9'])
let second = (['0'-'5']['0'-'9'])
let msec = ['0'-'9']['0'-'9']['0'-'9']
let call_id = ['A'-'F' '0'-'9']
let version = ['A'-'Z' '0'-'9' '.' '_' '-']
let priority = ("DEBUG"|"INFO"|"WARN"|"ERROR")
let funcname = [^ '|' '\n']
let data = [^ '|' '\n']

let node = [^ '<']
let ident = [^ '\n']
 
rule entry_line = parse 
    | (year as yr) '-' (month as mn) '-' (day as dy) 'T' 
      (hour as hh) ':' (minute as mm) ':' (second as ss) ',' 
      (msec as ms) '|' (call_id* as id) '|' (version+ as ver) '-'
      (priority as pri) [' ']* '|' (funcname* as func) '|' 
      {
        {
            entry_date = Date.of_strs yr mn dy;
            entry_time = Time.of_strs hh mm ss;
            entry_msec = int_of_string ms;
            entry_id   = id;
            entry_vers = ver;
            entry_prio = PriSer.of_string pri;
            entry_func = func;
            entry_data = data lexbuf
        }
      }

and entry_test = parse 
    | (year as yr) '-' (month as mn) '-' (day as dy) 'T' 
      (hour as hh) ':' (minute as mm) ':' (second as ss) ',' 
      (msec as ms) '|' (call_id* as id) '|' (version+ as ver) '-'
      (priority as pri) [' ']* '|' (funcname* as func) '|'
      {
        {
            entry_date = Date.of_strs yr mn dy;
            entry_time = Time.of_strs hh mm ss;
            entry_msec = int_of_string ms;
            entry_id   = id;
            entry_vers = ver;
            entry_prio = PriSer.of_string pri;
            entry_func = func;
            entry_data = data lexbuf
        }
      }

and data = parse
    | "chaining from >" (node+ as nfrom) "< to >" (node+ as nto) "<" {
            Link { link_from = nfrom; link_to = nto }
        }
    | "Label: " (ident* as lbl) {
            Label { label = lbl }
        } 
    | (ident* as stt) ": entering state" {
            State { state = stt }
        }
    | (ident* as other) {
            Other other
        }

{
let rec lineno = ref 0
let rec input_entry fin = 
    incr lineno;
    match Tools.input_line fin with
    | None -> None
    | Some line when Pcre.pmatch ~rex:entry_rex line -> read_entry fin line
    | Some line -> input_entry fin

and read_entry fin line = 
    let entry = entry_line (Lexing.from_string line) in
    match entry.entry_data with
    | Other _ -> input_entry fin
    | x -> Some x

let input_channel fin = 
    let rec aux ls = 
        match input_entry fin with
        | None -> ls
        | Some e -> aux (e :: ls)
    in
    aux []

let input_file fname = Tools.with_in_file input_channel fname 

let line = "2025-08-25T05:24:36,068|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|audio.welcid0305_PlayWelcome_PP|genericInterceptFOWelcome: "
let eline = "Address: https://cct-rest.est.k8s.qa.comm.spctrm.net/account-search/v2/search?ucid=1299376E68AC2BC1&ani=9542376966&ced=&accountNumber=&dnis=11071905113&siteId="
let linkline = "2025-08-25T05:24:39,766|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >welcid0305_PlayWelcome_PP< to >welcid0305_PlayWelcome_PP_DS<"
let labelline = "2025-08-25T05:24:39,766|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: PlayCallMonitoring"


}

