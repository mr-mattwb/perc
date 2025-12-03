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
 
rule line = parse 
    | (year as yr) '-' (month as mn) '-' (day as dy) 'T' 
      (hour as hh) ':' (minute as mm) ':' (second as ss) ',' 
      (msec as ms) '|' (call_id* as id) '|' (version+ as ver) '-'
      (priority as pri) '|' (funcname* as func) '|' 
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

