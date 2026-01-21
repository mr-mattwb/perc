{
    open Unix
    open Printf
    open Stdlib

    type priority = 
        | DEBUG
        | INFO
        | WARN
        | ERROR

    type entry = {
        date : Date.t;
        time : Time.t;
        msec : int;
        iden : string;
        vers : string;
        prio : priority;
        func : string;
        data : data
    }
    and data = 
        | Link of link
        | Label of string
        | State of string
        | NextModule of string
        | ReturnNode of string
        | LastVisitedModule of string
        | Portal of string
        | RoutingDS of routing_ds
        | ReturnValue of string
        | NodeHostname of string
        | CallInfo of call_info
        | Other of string

    and link = {
        link_from : string;
        link_to : string
    }
    and routing_ds = {
        returnValue : string;
        identifiedFlag : bool;
        lastVisitedModule : string;
        appTag : string;
        intentIntercept : string;
        dialedPortalName : string
    }
    and call_info = {
        dnis : string;
        ani : string;
        ucid : string;
        firstHistoryInfoUser : string;
        lastHistoryInfoUser : string;
        receivedUcid : string option;
        receivedUui : string option
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
                | _ -> raise (Failure ("Invalid priority ["^s^"]"))
            let to_string = function
                | DEBUG -> "DEBUG"
                | INFO -> "INFO"
                | WARN -> "WARN"
                | ERROR -> "ERROR"
        end
    let bool_of_string b = 
        match String.uppercase_ascii b with
        | "TRUE" | "T"| "YES" | "Y" | "1" -> true
        | _ -> false
}
let dig = ['0'-'9']
let nat = ['1'-'9']
let year = ['1' '2'] dig dig dig
let month = '0'['1'-'9']|'1'['0'-'2']
let day = '0'['1'-'9']|'3'['0'-'1']|['1'-'2']['0'-'9']
let hour = ['0'-'1']['0'-'9']|'2'['0'-'3']
let minute = ['0'-'5']['0'-'9']
let second = ['0'-'5']['0'-'9']
let msec = dig dig dig
let hexval = ['0'-'9' 'A'-'F' 'a'-'f']
let hex = hexval
let hexvals = hexval*
let version = [^'-']*
let priority = "DEBUG"|"INFO"|"WARN"|"ERROR"
let ident = ['A'-'z' '0'-'9' '_' ]+
let boolval = "true"|"false"|"yes"|"no"|"1"|"0"
let nobracks = [^']']*
let nospaces = [^' ']*
let hexnull = hexval+|"null"

rule entry = parse
    | (year as yr) '-' (month as mo) '-' (day as da)
        'T' (hour as hr) ':' (minute as mi) ':' (second as se)
        ',' (msec as ms) '|' (hexvals as id) '|' (version as vers) 
        '-' (priority as prio) [' ']* '|' ([^'|']+ as func) '|' {
            {
                date = Date.of_strs yr mo da;
                time = Time.of_strs hr mi se;
                msec = int_of_string ms;
                iden = id;
                vers = vers;
                prio = (PrioSer.of_string prio);
                func = func;
                data = data lexbuf
            }
        }
and data = parse
    | "chaining from >" (ident as nfrom) "< to >" (ident as nto) "<" {
            Link { 
                link_from = nfrom; 
                link_to = nto 
            }
        }
    | "Label: " (_+ as label) {
            Label label
        }
    | (_+ as func) ": entering state" {
            State func
        }
    | "nextModule: [" ([^']']* as nxt) "]" {
            NextModule nxt
        }
    | "Return[" ([^']']* as node) "]" {
            ReturnNode node
        }
    | "lastVisitedModule" ' '? ": " '['? (_* as lvm) ']'? {
            LastVisitedModule lvm
        }
    | "portalName: [" ([^']']* as pn) "]" {
            Portal pn
        }
    | "intent0110_Routing_DS: returnValue[" ([^']']* as rval) "] identifiedFlag[" (boolval as idf) "] lastVisitedModule["
        ([^']']* as lvm) "] appTag[" ([^']']* as apptag) "] intentIntercept[" ([^']']* as iint) "]dialedPortalName["
        ([^']']* as dpn) "]" {
            RoutingDS {
                returnValue = rval;
                identifiedFlag = bool_of_string idf;
                lastVisitedModule = lvm;
                appTag = apptag;
                intentIntercept = iint;
                dialedPortalName = dpn
            }
        }
    | "returnValue:[" ([^']']* as rv) "]" {
            ReturnValue rv
        }
    | "NODE_HOSTNAME [" (nobracks as hn) "]" {
            NodeHostname hn
        }
    | "DNIS :" (dig+ as dnis) " ANI :" (dig+ as ani) " UCID :" (hex+ as ucid) " FIRSTHISTORYINFOUSER :" 
        (dig+ as fhiu) " LASTHISTORYINFOUSER :" (dig+ as lhiu) " RECEIVED_UCID :" (hexnull+ as rucid) 
        " RECEIVED_UUI :" (hexnull+ as ruui) {
            CallInfo {
                dnis = dnis;
                ani = ani;
                ucid = ucid;
                firstHistoryInfoUser = fhiu;
                lastHistoryInfoUser = lhiu;
                receivedUcid = (match rucid with "null" -> None | x -> Some x);
                receivedUui = (match ruui with "null" -> None | x -> Some x)
            }
        }

and date = parse
    | (year as yr) '-' (month as mo) '-' (day as da)  { Date.of_strs yr mo da }
and time = parse
    | (hour as hr) ':' (minute as mi) ':' (second as se) { Time.of_strs hr mi se }


{
    let parse_entry str = 
        try
            Some (entry (Lexing.from_string str))
        with _ -> 
            None
}
