{
open Unix
open Printf
open Stdlib

open PLogBase

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
          PLogBase.of_strs (yr,mon,day) (hh, mm, ss) msec id ver pri func rest 
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
              prio = PLogBase.PriSer.of_string pri;
              func = f;
              data = data lexbuf
            }
          }

and priority = parse
    | "INFO"        { INFO      }
    | "DEBUG"       { DEBUG     }
    | "WARN"        { WARN      }
    | "ERROR"       { ERROR     }
