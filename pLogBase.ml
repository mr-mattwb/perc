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

