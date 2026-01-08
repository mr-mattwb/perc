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

    type intentRoutingDS = {
        returnValue : string;
        identifiedFlag : bool;
        lastVisitedModule  : string;
        appTag : string;
        intentIntercept : string;
        dialedPortalName : string
    }
    type callinfo = {
        dnis : string option;
        ani : string option;
        ucid : string option;
        firstHistInfoUser : string option;
        lastHistInfoUser : string option;
        receivedUcid : string option;
        receivedUui : string option;
    }

    type callType =
        | Residential
        | Commercial
        | Business

    type customer = {
        ucid : string;
        ani : string;
        ced : string;
        acctNumber : string;
        dnis : string;
        siteId : string
    }

    type auth = {
        eligible : bool;
        voiceBio : bool
    }

    type init_config = {
        name : string;
        path : Tools.file
    }

    type invocation = 
        | Start
        | End

    type xfer_msg = {
        routingCode : string;
        lastVisitedModule : string;
        transferMsgPlayed : bool;
        portalName : string;
        identified : bool;
        delinquentLevel : string;
        genericInterceptFO : bool;
        retryZipCode : bool;
        botEligible : bool;
        oof : bool;
        isPlayFreeSpecmoNotice : bool;
        callType : callType;
        preAuthLastModule : string
    }

    type profile_acct = {
        acct_no : string;
        status : string
    }

    type business_unit = {
        siteID : string;
        businessUnit : string;
        returnCode : string
    }

    type direction = 
        | Enter
        | Exit

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
        | IntentRoutingDS of intentRoutingDS
        | InitReturnValue of string
        | NodeHostname of string
        | CallInfo of callinfo
        | Language of string
        | Dnis of string
        | BusinessUnit of string
        | CallType of callType
        | Ani of string
        | Customer of customer
        | AccountNumber of string
        | Authentication of auth
        | CategoryCode of int
        | InitConfiguration of init_config
        | InvocationCounter of invocation
        | CatCodesSalesTransfer of int list
        | PlayTransferMsg of xfer_msg
        | CatCodeEligibleSalesTransfer of bool
        | ReturnValue of string
        | CreatedToken of string 
        | ANILookup of string
        | DTMFOnly of bool
        | CableProfileAcct of profile_acct
        | Identified of bool
        | IsVisitedNode of string * bool
        | BusinessUnitReturnCode of string
        | BusinessUnitInfo of business_unit
        | Method of direction * string
        | Other of string

    let string_of_null_string = function
        | "" | "null" -> None
        | s -> Some s

    let bool_of_string s = 
        match String.uppercase_ascii s with
        | "FALSE" | "NO" | "T" | "N" | "0" -> false
        | "TRUE" | "YES" | "F" | "Y" | "1" -> true
        | _ -> false

    let callType_of_string s = 
        match String.uppercase_ascii s with
        | "RESIDENTIAL" -> Residential
        | "COMMERCIAL" -> Commercial
        | "BUSINESS" -> Business
        | _ -> raise (Failure ("Cannot convert to call type ["^s^"]"))
        
    type 'a call = {
        call_iden : string;
        call_nodes : 'a list
    }
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
let boolval = ("true"|"false")

let nums = ("null"|['0'-'9']*)
let hexes = ("null"|['0'-'9' 'A'-'F']*)

let lang = ['a'-'z' 'A'-'Z' '-' '.' '_' ]

let hexes2 = ['0'-'9' 'A'-'F' 'a'-'f']
let nums2 = ['0'-'9']
let businessUnit = ['A'-'Z' 'a'-'z' '0'-'9']
let callType = ("Residential"|"Commercial"|"Business")

let authstr = ("true"|"false"|"yes"|"no"|"T"|"F"|"Y"|"N"|"0"|"1")

let namestr = [ 'A'-'Z' 'a'-'z' '0'-'9' ]
let pathstr = [ 'A'-'Z' 'a'-'z' '0'-'9' '-' '_' '/' ]

let url = ['A'-'Z' 'a'-'z' '0'-'9' '.' '&' '?' '%' '=' '/' ':' '_' '-' ]


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
    | "saveUtteranceToAudioHandleList: stateName: " (func+ as sn) {
            StateName sn
        }
    | "intent0110_Routing_DS: returnValue[" (func* as rv) "] identifiedFlag[" (boolval as idf) 
        "] lastVisitedModule[" (func* as lvm) "] appTag[" (func* as apptag) "] intentIntercept["
        (func* as ii) "]dialedPortalName[" (func* as dpn) "]" {
            IntentRoutingDS {
                returnValue = rv;
                identifiedFlag = bool_of_string idf;
                lastVisitedModule = lvm;
                appTag = apptag;
                intentIntercept = ii;
                dialedPortalName = dpn
            }
        }
    | "returnValue:[" (func* as id) "]" {
            InitReturnValue id
        }
    | "NODE_HOSTNAME [" (func* as id) "]" {
            NodeHostname id
        }
    | "DNIS :" (nums* as dnis) " ANI :" (nums* as ani) " UCID :" (hexes as ucid) 
        " FIRSTHISTORYINFOUSER :" (nums as fhiu) " LASTHISTORYINFOUSER :" (nums as lhiu)
        " RECEIVED_UCID :" (hexes* as rucid) " RECEIVED_UUI :" (hexes* as ruui) {
            CallInfo {
                dnis = string_of_null_string dnis;
                ani =  string_of_null_string ani;
                ucid =  string_of_null_string ucid;
                firstHistInfoUser =  string_of_null_string fhiu;
                lastHistInfoUser =  string_of_null_string lhiu;
                receivedUcid = string_of_null_string rucid; 
                receivedUui = string_of_null_string ruui
            }
        }
    | "language:[" (lang+ as lng) "]" {
            Language lng
        }
    | "ani:[" (nums2* as ani) "]" {
            Ani ani
        }
    | "dnis:[" (nums2* as dnis) "]" {
            Dnis dnis
        }
    | "businessUnit:[" (businessUnit* as bu) "]" {
            BusinessUnit bu
        }
    | "callType:[" (callType as ct) "]" {
            CallType (callType_of_string ct)
        }
    | "Executing findCustomerUsingGET ucid[" (hexes2* as ucid) "] ani[" (nums2* as ani) "] ced[" (nums2* as ced) 
        "] accountNumber[" (nums2* as acctNum) "] dnis[" (nums2* as dnis) "] siteId[" (func* as siteId) "]" {
            Customer {
                ucid = ucid;
                ani = ani;
                ced = ced;
                acctNumber = acctNum;
                dnis = dnis;
                siteId = siteId
            }
        }
    | "accountNumber: " (nums2* as acctnum) {
            AccountNumber acctnum
        }
    | "authenticationEligible:[" (authstr as elig) "],voiceBioEnrolled:[" (authstr as vbio) "]" {
            Authentication {
                eligible = bool_of_string elig;
                voiceBio = bool_of_string vbio
            }
        }
    | "categoryCode: " (nums2+ as catcode) {
            CategoryCode (int_of_string catcode)
        }
    | "initConfiguration: " (namestr+ as name) " : " (pathstr+ as path) {
            InitConfiguration {
                name = name;
                path = path
            }
        }
    | "InvocationCounter.valueUnbound: call" (("start"|"end") as dir) " was called [0]  times but it should have been called exactly once"  {
            InvocationCounter (match dir with "start" -> Start | _ -> End)
        }
    | "catCodesForSalesTransfer " {
            CatCodesSalesTransfer (categoryCodeList lexbuf)
        }
    | "routingCode[" (func* as routingCode) "] lastVisitedModule[" (func* as lastVisitedModule) "] "
      "transferMsgPlayedFlag[" (boolval as transferMsgPlayed) "] portalName[" (func* as portalName) "] "
      "identifiedFlag[" (boolval as identified) "] delinquientLevel[" (func* as delinquentLevel) "] " 
      "genericInterceptFOFlag[" (boolval as genericInterceptFO) "] retryZipCode[" (boolval as retryZipCode) "] "
      "botEligible[" (boolval as botEligible) "] oofFlag[" (boolval as oof) "] isPlayFreeSpecmoNotice[" (boolval as isPlayFreeSpecmoNotice) "] "
      "callType[" (callType as callType) "] preAuthLastModule[" (func* as preAuthLastModule) "]" {
          PlayTransferMsg {
            routingCode = routingCode;
            lastVisitedModule = lastVisitedModule;
            transferMsgPlayed = bool_of_string transferMsgPlayed;
            portalName = portalName;
            identified = bool_of_string identified;
            delinquentLevel = delinquentLevel;
            genericInterceptFO = bool_of_string genericInterceptFO;
            retryZipCode = bool_of_string retryZipCode;
            botEligible = bool_of_string botEligible;
            oof = bool_of_string oof;
            isPlayFreeSpecmoNotice = bool_of_string isPlayFreeSpecmoNotice;
            callType = callType_of_string callType;
            preAuthLastModule = preAuthLastModule
          }
        }
    | "catCodeEligigbleForSalesTransfer: " (boolval as elig) { 
            CatCodeEligibleSalesTransfer (bool_of_string elig)
        }
    | "welcid0705_GetPhoneAccountNumber_DM returnValue >" (func* as id) "<" {
            ReturnValue id
        }
    | "Created _TOKEN [" (func+ as token) "]" {
            CreatedToken token
        }
    | "[" (hexes2* as iden) "] Performing ANI Lookup" {
            ANILookup iden
        }
    | "dtmfOnly:[" (boolval as dtmfonly) "]" {
            DTMFOnly (bool_of_string dtmfonly)
        }
    | "intent0110_Routing_DS: cableProfile (accountNumber[" (nums2* as acctno) "] accountStatus[" (func* as status) "])" {
            CableProfileAcct {
                acct_no = acctno;
                status = status
            }
        }
    | "identifiedFlag: [" (boolval as id) "]" {
            Identified (bool_of_string id)
        }
    | "isVisitedNode(" (func+ as func) "): " (boolval as visited) {
            IsVisitedNode(func, bool_of_string visited)
       }
    | "businessUnitReturnCode : " (func* as rc) {
            BusinessUnitReturnCode rc
        }
    | "BusinessUnitInfo value: BusinessUnitInfo [siteID=" (nums2* as siteID) ", businessUnit=" (func* as busUnit)
        ", returnCode=" (func* as retCode) "]"  {
            BusinessUnitInfo {
                siteID = siteID;
                businessUnit = busUnit;
                returnCode = retCode
            }
        }
    | (func+ as meth) ": " ("Enter"|"Exit" as dir) "ing " ['M' 'm'] "ethod." {
            Method ((match dir with "Enter" -> Enter | "Exit" -> Exit | _ -> Exit), meth)
        }
    | _+ as other { Other other }

and categoryCodeList = parse
    | (nums2+ as code) ","                      { (int_of_string code) :: (categoryCodeList lexbuf) }
    | (nums2+ as code)                          { (int_of_string code) :: [] }
    | eof                                       { [] }
        
{

let parse_entry line = 
    try
        Some (entry (Lexing.from_string line))
    with Failure msg when msg = "lexing: empty token"->
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

module Ids = Set.Make(String)
let get_ids entries = 
    let aux set e = 
        match e.entry_iden with
        | "" -> set
        | id -> Ids.add id set 
    in
    Ids.elements (List.fold_left aux Ids.empty entries)
let get_call id entries = 
    let aux acc v = 
        if v.entry_iden = id then v :: acc
        else acc
    in
    {
        call_iden = id;
        call_nodes = List.fold_left aux [] entries
    }
let get_calls entries = 
    let aux id = get_call id entries in
    List.map aux (get_ids entries)

let input_calls fname = get_calls (input_file fname)
}
