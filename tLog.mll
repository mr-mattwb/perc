{
open Unix
open Printf
open Stdlib

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

module PrioSer = 
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

    type link = {
        link_from : string;
        link_to : string
    }

    type data =
        | Link of link
        | Label of string
        | EnterState of string
        | ExitState of string
        | NextModule of string
        | Return of string
        | LastVisited of string
        | Portal of string
        | StateName of string
        | Other of string
}

let year = ['1'-'2']['0'-'9']['0'-'9']['0'-'9']
let month = ('0'['1'-'9'])|('1'['0'-'2'])
let day =  ('0'['1'-'9'])|('3'['0'-'1'])|(['1'-'2']['0'-'9'])

let hour = (['0'-'1']['0'-'9'])|('2'['0'-'3'])
let minute = ['0'-'5']['0'-'9']
let second = ['0'-'5']['0'-'9']
let msec = ['0'-'9']['0'-'9']['0'-'9']

let iden = ['A'-'F' '0'-'9']
let vers = ['A'-'Z' '0'-'9' '.' '_']
let prio = ("DEBUG"|"WARN"|"INFO"|"ERROR")

let func = [^ '|' '<' '>' ']']

rule entry = parse
    | [ ' ' '\t' ]+                                         { entry lexbuf } 
    | (year as yr) '-' (month as mn) '-' (day as dy) 'T'       
      (hour as hr) ':' (minute as mi) ':' (second as sc) ','
      (msec as ms) '|' (iden* as id) '|' (vers+ as vs) '-'
      (prio as pr) [' ']* '|' (func* as fn) '|'
      { 
        {
            entry_date = Date.of_strs yr mn dy;
            entry_time = Time.of_strs hr mi sc;
            entry_msec = int_of_string ms;
            entry_iden = id;
            entry_vers = vs;
            entry_prio = PrioSer.of_string pr;
            entry_func = fn;
            entry_data = data lexbuf
        }
      }
and data = parse
    | "chaining from >" (func* as nfrom) "< to >" (func* as nto) "<"  {
            Link { 
                link_from = nfrom;
                link_to = nto
            }
        }
    | "Label: " (_* as lbl) {
            Label lbl
        }
    | (_* as st) ": entering state" {
            EnterState st
        }
    | (_* as st) ": exiting state" {
            ExitState st
        }
    | "nextModule: [" (func+ as next) "]" {
            NextModule next
        }
    | "Return[" (func+ as rtn) "]" {
            Return rtn
        }
    | "lastVisitedModule: [" (func+ as lvm) "]"  {
            LastVisited lvm
        }
    | "portalName: [" (func+ as pn) "]" {
            Portal pn
        }
    | "stateName: " (func+ as sn) {
            StateName sn
        }
    | (_* as oth) {
            Other oth
        }
        
{
let linkline = "2025-08-25T10:59:33,343|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >end2025_BackendLogging_DB< to >end2030_ReportingLogging_DB<"
let labelline = "2025-08-25T10:59:32,005|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: StartSession failure"
let stateline = "2025-08-25T05:28:20,408|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state"
let otherline = "2025-08-25T05:28:20,408|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|welcid0100_Dummy_DM returnValue ><"
let nextline = "2025-08-25T05:30:09,175|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|decision.controller_return_DS|nextModule: [actall]"
let returnline = "2025-08-25T05:38:24,123|DF9941BC970FB64FF98CB929644CD997|MOD25.09.0.004-INFO |reporting.CDRDialogModuleState|Return[portal0225_GenericInterceptQuestion_DM_No_DS]"
let lastvisitedline = "2025-08-25T05:49:19,893|B65B8A99CF70FD9AA3F29042C65B820B|MOD25.09.0.004-DEBUG|decision.portal0110_Routing_DS|lastVisitedModule: [start]"
let portalline = "2025-08-25T06:01:11,409|5115444DD196B688BBA0139DBF7354D5|MOD25.09.0.004-DEBUG|decision.welcid0705_GetPhoneAccountNumber_DM_DS|portalName: [MainResidential]"
let stateline2 = "2025-08-25T06:05:05,512|87506A078C3752E95DE004603FD6BAEB|MOD25.09.0.004-DEBUG|reporting.CDRUtil|saveUtteranceToAudioHandleList: stateName: welcid0510_ConfirmAniYN_DM"

let parse_entry line = 
    try
        Some (entry (Lexing.from_string line))
    with Failure _ ->
        None

let rec input_entry fin = 
    let aux line = 
        match parse_entry line with
        | None -> input_entry fin
        | Some e -> Some e
    in
    match Tools.input_line fin with
    | None -> None
    | Some line -> aux line

let rec read_entry fin = 
    let aux e = 
        match e.entry_data with
        | Other _ -> read_entry fin
        | _ -> Some e
    in
    match input_entry fin with
    | None -> None
    | Some e -> aux e

let rec input_channel fin = 
    let rec aux ls = 
        match input_entry fin with
        | Some e -> aux (e :: ls)
        | None -> ls
    in
    aux []

let rec input_file fname = Tools.with_in_file input_channel fname

let clean ls = 
    let aux acc e = 
        match e.entry_data with
        | Other _ -> acc
        | _ -> e :: acc
    in
    List.fold_left aux [] ls

}
