{
    open Unix
    open Printf
    open Stdlib

    type priority = 
        | DEBUG
        | INFO
        | WARN
        | ERROR

    module PortalMap = Map.Make(Int)
    type portalmap = string PortalMap.t

    module CatCodeTable = Map.Make(String)
    type catcode_table = int CatCodeTable.t

    module BusinessUnitTable = Map.Make(String)
    type business_unit_table = string BusinessUnitTable.t

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
        | Language of string
        | Dnis of string
        | Ani of string
        | BusinessUnit of string
        | CallType of call_type
        | CustomerInfo of customer_info
        | Authentication of authentication
        | InitConfiguration of init_config
        | Invocation of invoke_dir
        | CatCodesForSalesTransfer of int list
        | VisitedNode of string * bool
        | AccountStatus of account_status
        | BusinessUnitInfo of business_unit_info
        | InitServiceConfiguration of dir
        | PortalMap of portalmap
        | CallFlowConfig of call_flow_config
        | CategoryCodeTable of catcode_table
        | BusinessUnitTable of business_unit_table
        | NullConfigValue of call_flow_config
        | LocalFileRepository of string
        | DBExecuting of string
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
    and call_type = 
        | Residential
        | Commercial
        | Business
    and customer_info = {
        ucid : string;
        ani : string;
        ced : string;
        acct : string;
        dnis : string;
        siteId : string
    }
    and authentication = {
        eligible : bool;
        voiceBio : bool
    }
    and init_config = {
        name : string;
        path : string
    }
    and invoke_dir =
        | Start
        | End
    and account_status = {
        accountNumber : string;
        accountStatus : char
    }
    and business_unit_info = {
        siteId : string;
        businessUnit : string;
        returnCode : string
    }
    and dir = 
        | Enter
        | Exit
    and call_flow_config = {
        callFlow : string;
        path : string
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
    let callType_of_string s = 
        match String.uppercase_ascii s with
        | "RESIDENTIAL" -> Residential
        | "COMMERCIAL" -> Commercial
        | "BUSINESS" -> Business
        | _ -> raise (Failure ("Invalid callType ["^s^"]"))
    let dir_of_string = function
        | "start" -> Start
        | "end" -> End
        | x -> raise (Failure ("Invoalid invocation direction ["^x^"]"))
    let position_of_string = function
        | "Enter" -> Enter
        | "Exit" -> Exit
        | p -> raise (Failure ("Invalid direction ["^p^"]")) 
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
let callType = "Residential"|"Commercial"|"Business"
let dir = "start"|"end"
let alpha = ['A'-'z']
let upperCaseNum = ['A'-'Z' '0'-'9']
let portalName = ['A'-'z' '0'-'9' '-' '_' ]
let ccName = ['A'-'z' '0'-'9' '_' '-' ]

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
    | "language:[" (nobracks as lang) "]" {
            Language lang
        }
    | "dnis:[" (nobracks as dnis) "]" {
            Dnis dnis
        }
    | "ani:[" (nobracks as ani) "]" {
            Ani ani
        }
    | "businessUnit:[" (nobracks as bu) "]" {
            BusinessUnit bu
        }
    | "callType:[" (callType as ct) "]" {
            CallType (callType_of_string ct)
        }
    | "Executing findCustomerUsingGET ucid[" (hex+ as ucid) "] ani[" (dig+ as ani) "] ced[" (nobracks as ced) 
        "] accountNumber[" (nobracks as acct) "] dnis[" (dig+ as dnis) "] siteId[" (nobracks as sid) "]" {
            CustomerInfo {
                ucid = ucid;
                ani = ani;
                ced = ced;
                acct = acct;
                dnis = dnis;
                siteId = sid
            }
        }
    | "authenticationEligible:[" (boolval as elig) "],voiceBioEnrolled:[" (boolval as voiceBio) "]" {
            Authentication {
                eligible = bool_of_string elig;
                voiceBio = bool_of_string voiceBio
            }
        }
    | "initConfiguration: " (nospaces as name) " : " (_+ as path) {
            InitConfiguration {
                name = name;
                path = path
            }
        }
    | "InvocationCounter.valueUnbound: call" (dir as dir) " was called [0]  times but it should have been called exactly once" {
            Invocation (dir_of_string dir)
        }
    | "catCodesForSalesTransfer: " {
            CatCodesForSalesTransfer (int_comma_list lexbuf)
        } 
    | "isVisitedNode("([^')']+ as node)"): " (boolval as visited) {
            VisitedNode (node, bool_of_string visited)
        }
    | "intent0110_Routing_DS: cableProfile (accountNumber["(dig+ as acctno)"] accountStatus["(alpha as acctst)"])" {
            AccountStatus {
                accountNumber = acctno;
                accountStatus = acctst
            }
        }
    | "BusinessUnitInfo value: BusinessUnitInfo [siteID="(dig+ as sid)", businessUnit="(alpha+ as bu)", returnCode="(alpha+ as rc)"]" {
            BusinessUnitInfo {
                siteId = sid;
                businessUnit = bu;
                returnCode = rc
            }
        }
    | ".initServiceConfiguration: "("Enter"|"Exit" as pos)"ing "['m''M']"ethod"'.'? {
            InitServiceConfiguration (position_of_string pos)
        }
    | "PortalMap["(_+ as pmap)"]" {
            PortalMap (portalMap (Lexing.from_string pmap))
        }
    | "DB Config Flag Map is Empty for Config ID: "(alpha+ as cfName)" with fileName: "(_+ as fname) {
            CallFlowConfig {
                callFlow = cfName;
                path = fname
            }
        }
    | "categoryCodeTable={"(_* as tbl)"}" {
            CategoryCodeTable (categoryCodeTable (Lexing.from_string tbl))
        }
    | "businessUnitTable={"(_* as tbl)"}" {
            BusinessUnitTable (businessUnitTable (Lexing.from_string tbl))
        }
    | "Null value for "(alpha+ as cfVal)" in fileName: "(_+ as fileName) {
            NullConfigValue {
                callFlow = cfVal;
                path = fileName
            }
        }
    | "loading configuration from local file repository: "(_+ as fileName) {
            LocalFileRepository fileName
        }
    | "executing >"(_+ as sql)"<" {
            DBExecuting sql
        }
    
and businessUnitTable = parse
    | eof                           { BusinessUnitTable.empty }
    | [' ']*','[' ']*               { businessUnitTable lexbuf }
    | (upperCaseNum+ as num)'='(upperCaseNum+ as name)  {
            BusinessUnitTable.add num name (businessUnitTable lexbuf)
        }
and categoryCodeTable = parse
    | [' ']*','[' ']*               { categoryCodeTable lexbuf }
    | eof                           { CatCodeTable.empty }
    | (ccName+ as name)'='(dig+ as num) { 
            CatCodeTable.add name (int_of_string num) (categoryCodeTable lexbuf)
        }
and portalMap = parse
    | '{'                           { portalMap lexbuf }
    | '}'                           { PortalMap.empty }
    | ", "                          { portalMap lexbuf }
    | (dig+ as pnum)'='(portalName+ as pname) {
            PortalMap.add (int_of_string pnum) pname (portalMap lexbuf)
        }

and int_comma_list = parse
    | ","                           { int_comma_list lexbuf }
    | (dig+ as icl)                 { (int_of_string icl) :: (int_comma_list lexbuf) }
    | eof                           { [] }

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

    let input_channel fin = 
        let aux ls = 
            match Tools.input_line fin with
            | None -> ls
            | Some line -> aux (parse ls line)
        and parse ls line = 
            match parse_entry line with
            | Some e -> e :: ls
            | None -> ls
        in
        aux []

    let input_entries fname = 
        Tools.with_in_file input_channel fname
}

