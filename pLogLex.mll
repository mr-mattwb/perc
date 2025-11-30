{
open Unix
open Printf
open Stdlib

type id = string

type priority = 
    | INFO
    | DEBUG
    | WARN
    | ERROR

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : int;
    id : id;
    ivr : string;
    prio : priority;
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

module PriSer = 
    struct
        type elt = priority
        let of_string = function
            | "DEBUG" -> DEBUG
            | "INFO"  -> INFO
            | "WARN"  -> WARN
            | "ERROR" -> ERROR
            | x -> raise (Failure ("Cannot conver to priority ["^x^"]"))
        let to_string = function
            | DEBUG   -> "DEBUG"
            | INFO    -> "INFO"
            | WARN    -> "WARN"
            | ERROR   -> "ERROR"
    end

let of_strs (yr, mn, dy) (hh, mm, ss) ms id ver pri func data = {
    date = Date.of_strs yr mn dy;
    time = Time.of_strs hh mm ss;
    msec = int_of_string ms;
    id = id;
    ivr = ver;
    prio = PriSer.of_string pri;
    func = func;
    data = data
}

let datefmt = "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)"
let timefmt = "(\\d\\d):(\\d\\d):(\\d\\d)"
let msecfmt = "(\\d\\d\\d)"
let idfmt = "([A-F0-9]+)"
let priofmt = "([A-Z0-9\\.]+)-([A-Z]+)"
let funcfmt = "([a-zA-Z0-9\\.\\_]+)"
let datafmt = "(.*)"

let entry_pat = datefmt ^ "T" ^ timefmt ^ "," ^ msecfmt ^ "\\|" ^ idfmt ^ "\\|" ^ priofmt ^ "\\|" ^ funcfmt ^ "\\|"
let link_pat = entry_pat ^ "chaining from >(.*)< to >(.*)<"
let label_pat = entry_pat ^ "Label: (.*)"
let state_pat = entry_pat ^ "(.*): entering state"
let data_pat = entry_pat ^ "(.*)"

let entry_match line = Pcre.pmatch ~pat:data_pat line
let link_match line  = Pcre.pmatch ~pat:link_pat line
let label_match line = Pcre.pmatch ~pat:label_pat line
let state_match line = Pcre.pmatch ~pat:state_pat line

}

let year = ['1'-'2']['0'-'9']['0'-'9']['0'-'9']
let month = ("0" ['1'-'9'] | "1" ['0'-'2'])
let day = ("0" ['1'-'9'] | ['1'-'2'] ['0'-'9'] | "3" ['0'-'1']) 
let hour = (['0'-'1'] ['0'-'9'] | "2" ['0'-'3'])
let minute = (['0'-'5'] ['0'-'9']) 
let second = (['0'-'5'] ['0'-'9'])
let msec = ['0'-'9']['0'-'9']['0'-'9']

let callid = ['0'-'9' 'A'-'F']
let version = ['0'-'9' 'A'-'Z' '.']
let priority = ['A'-'Z']
let func = ['A'-'Z' 'a'-'z' '0'-'9' '.' '_']
let rest = [^ '\n']*
let token = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let identifier = [^ '\n']
let field = [^ '|']

let eoln = '\n'

rule entry = parse
    | (year as yr) '-' (month as mon) '-' (day as day)  
      'T' (hour as hh) ':' (minute as mm) ':' (second as ss)
      ',' (msec as msec)
    '|' (callid* as id)
    '|' (version+ as ver)
    '-' (priority+ as pri) [' ']*
    '|' (func* as func)        
    '|' { let rest = data lexbuf in
          of_strs (yr,mon,day) (hh, mm, ss) msec id ver pri func rest 
        }

and data = parse
    | "chaining from >" (token+ as nfrom)
      "< to >" (token+ as nto) "<"    { 
            Link { link_from = nfrom; link_to = nto } 
      }
    | "Label: " (identifier+ as label) {
            Label { label = label }
      }
    | (identifier+ as st) ": entering state" {
            State { state = st }
      }
    | (identifier* as st) { 
            Other st 
      }

and entrydata = parse
      (year as yr) '-' (month as mon) '-' (day as day)  
      'T' (hour as hh) ':' (minute as mm) ':' (second as ss) 
      ',' (msec as ms) 
      '|' (callid* as id) 
      '|' (version+ as ver) '-' (priority+ as pri) [' ']*
      '|' (func* as f) 
      '|' { { date = Date.of_strs yr mon day;
              time = Time.of_strs hh mm ss;
              msec = int_of_string ms;
              id   = id;
              ivr  = ver;
              prio = PriSer.of_string pri;
              func = f;
              data = data lexbuf
            }
          }

and priority = parse
    | "INFO"        { INFO      }
    | "DEBUG"       { DEBUG     }
    | "WARN"        { WARN      }
    | "ERROR"       { ERROR     }
