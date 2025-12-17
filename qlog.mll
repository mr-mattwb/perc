{
open Unix
open Printf
open Stdlib

let xyear = "([1-2][0-9][0-9][0-9])" 
let xmonth = "(0[0-9]|1[0-2])"
let xday = "(0[1-9]|3[0-1]|1[0-9]|2[0-9])"
let xhour = "(0[0-9]|1[0-9]|2[0-3])"
let xminute = "([0-5][0-9])"
let xsecond = "([0-5][0-9])"
let xdate = xyear^"-"^xmonth^"-"^xday
let xtime = xhour^":"^xminute^":"^xsecond
let xmsec = "[0-9][0-9][0-9]"
let xcallid = "[0-9A-Fa-f]*"
let xversion = "[0-9A-Z_\\.]+"
let xpriority = "(DEBUG|INFO|WARN|ERROR)"
let xfunc = "[A-Za-z_][0-9A-Za-z_\\.]*"

let xlink = "chaining from >(.*)< to >(.*)<"
let xlabel = "Label: (.*)"
let xstate = "(.*): entering state"
let xother = "(.*)"

let xentry = xdate^"T"^xtime^","^xmsec^"\\|"^xcallid^"\\|"^xversion^"-"^xpriority^"\\|"^xfunc^"\\|"^xother
let rexentry = Pcre.regexp xentry

type priority = 
    | DEBUG
    | INFO
    | WARN
    | ERROR

type 'a entry = {
    entry_date : Date.t;
    entry_time : Time.t;
    entry_msec : int;
    entry_iden : string;
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

type 'a call = {
    call_iden : string;
    call_nodes : 'a list
}

module SerPrio =
    struct
        type elt = priority
        let of_string s = 
            match String.uppercase_ascii s with
            | "DEBUG" -> DEBUG
            | "INFO" -> INFO
            | "WARN" -> WARN
            | "ERROR" -> ERROR
            | _ -> raise (Failure ("of_string("^s^")"))
        let to_string = function
            | DEBUG -> "DEBUG"
            | INFO -> "INFO"
            | WARN -> "WARN"
            | ERROR -> "ERROR"
    end

}

let year = ['1' '2'] ['0'-'9']['0'-'9']['0'-'9']
let month = ('0' ['0'-'9']) | ('1' ['0'-'2'])
let day = (['0'-'2'] ['0'-'9'])|('3' ['0'-'1'])
let hour = (['0'-'1']['0'-'9'])|('2' ['0'-'3'])
let minute = (['0'-'5']['0'-'9'])
let second = (['0'-'5']['0'-'9'])
let millisecs = ['0'-'9']['0'-'9']['0'-'9']
let callid = ['0'-'9' 'A'-'F' 'a'-'f' ]
let version = [ '0'-'9' 'A'-'Z' '_' '-' '.' ]
let priority = ("DEBUG"|"INFO"|"WARN"|"ERROR")
let funcname = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_' '.']*
let token = [ 'A'-'Z' 'a'-'z' '0'-'9' '_' ' ' '\t' ]

rule entry = parse
    | ((year "-" month "-" day) as date) "T" ((hour ":" minute ":" second) as time) ","
      (millisecs as msec) "|" (callid* as id) "|" (version+ as vers) "-" (priority+ as prio) "|"
      (funcname as func) "|" { 
        { entry_date = Date.parse_date date;
          entry_time = Time.parse_time time;
          entry_msec = int_of_string msec;
          entry_iden = id;
          entry_vers = vers;
          entry_prio = SerPrio.of_string prio;
          entry_func = func;
          entry_data = data lexbuf
        }
      }
and data = parse
    | "chaining from >" (token+ as nfrom)
    "< to >" (token+ as nto) "<" {
        Link { link_from = nfrom; link_to = nto }
    }
    | "Label: " (token+ as label) {
        Label { label = label }
    }
    | (token+ as state) ": entering state" {
        State { state = state }
    }
    | (_+ as data) {
        Other data
    }
{

let parse_entry line = entry (Lexing.from_string line)
let rec input_entry fin =
    match Tools.input_line fin with
    | None -> None
    | Some line when Pcre.pmatch ~rex:rexentry line -> Some (parse_entry line)
    | Some _ -> input_entry fin
let input_channel fin =
    let rec aux ls =
        match input_entry fin with
        | None -> ls
        | Some e -> aux (e :: ls)
    in
    aux []
let input_file fname = Tools.with_in_file input_channel fname

module Ids = Set.Make(String)
let get_ids entries = 
    let aux set e = Ids.add e.entry_iden set in
    Ids.elements (List.fold_left aux Ids.empty entries)
let get_call id entries = 
    let aux acc e = 
        if e.entry_iden = id then e :: acc
        else acc
    in
    {
        call_iden = id;
        call_nodes = List.fold_left aux [] entries
    }
let get_calls entries = 
    let aux acc id = (get_call id entries) :: acc in
    List.fold_left aux [] (get_ids entries)

let input_calls fname = 
    let entries = input_file fname in
    get_calls entries

module VLog = Log.Make(
    struct
        open Log
        let mod_name = Tools.basename
        let level () = Debug
        let targets () = [Channel stderr; File (Tools.basename^".log") ]
    end)
let verbose fmt = 
    let aux msg =
        if MEnv.Verbose.get() then
            VLog.debug "%s" msg
        else
            ()
    in ksprintf aux fmt

let main () = ()


let () = 
    if not !Sys.interactive then
        main ()
    else
        ()

}

