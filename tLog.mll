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
        | InvocationStart
        | InvocationEnd

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
    | "InvocationCounter.valueUnbound: callstart was called [0]  times but it should have been called exactly once"  {
            InvocationCounter InvocationStart
        }
    | "InvocationCounter.valueUnbound: callend was called [0]  times but it should have been called exactly once"  {
            InvocationCounter InvocationEnd
        }
    | "catCodesForSalesTransfer: " {
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


and categoryCodeList = parse
    | (nums2+ as code) ","                      { (int_of_string code) :: (categoryCodeList lexbuf) }
    | (nums2+ as code)                          { (int_of_string code) :: [] }
    | eof                                       { [] }
        
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
let intent0110line = "2025-08-25T06:05:55,707|87506A078C3752E95DE004603FD6BAEB|MOD25.09.0.004-DEBUG|decision.intent0110_Routing_DS|intent0110_Routing_DS: returnValue[task_general_bcm] identifiedFlag[true] lastVisitedModule[bcm] appTag[] intentIntercept[]dialedPortalName[]"
let returnValueline = "2025-08-25T10:58:47,558|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.Initialize|returnValue:[task_check_ani_match]"
let nodehostnameline = "2025-08-25T10:58:47,498|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.start0110_GetCallInfoFromWrapper_DS|NODE_HOSTNAME [comswmncwunxq07]"
let infouserline = "2025-08-25T10:58:47,498|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.start0110_GetCallInfoFromWrapper_DS|DNIS :11071905113 ANI :9542376966 UCID :119946ED68AC7A04 FIRSTHISTORYINFOUSER :8558955883 LASTHISTORYINFOUSER :8558955883 RECEIVED_UCID :null RECEIVED_UUI :null"
let languageline = "2025-08-25T10:58:47,501|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.languageSwitchOB|language:[en-US]"
let dnisline = "2025-08-25T05:34:47,451|50BE7C124CA2AA4580B0E6F38044AB4D|MOD25.09.0.004-DEBUG|decision.welcid0100_Initialize_DS|dnis:[11071900013]"
let businessUnitline = "2025-08-25T05:34:47,451|50BE7C124CA2AA4580B0E6F38044AB4D|MOD25.09.0.004-DEBUG|decision.welcid0100_Initialize_DS|businessUnit:[CSGEAST]"
let callTypeline = "2025-08-25T05:34:47,451|50BE7C124CA2AA4580B0E6F38044AB4D|MOD25.09.0.004-DEBUG|decision.welcid0100_Initialize_DS|callType:[Residential]"
let aniline = "2025-08-25T05:34:47,494|50BE7C124CA2AA4580B0E6F38044AB4D|MOD25.09.0.004-DEBUG|dataaccess.ANILookup|ani:[9940830410]"
let customerline = "2025-08-25T05:34:47,496||MOD25.09.0.004-DEBUG|client.AccountAndProfileSearchServiceClient|Executing findCustomerUsingGET ucid[1199290D68AC2E23] ani[9940830410] ced[] accountNumber[] dnis[11071900013] siteId[]"
let acctnumberline = "2025-08-25T07:00:43,045|4745BB4ED85C393E89D328AAB47ACB76|MOD25.09.0.004-DEBUG|dataaccess.data0307_AccountDetailsAppointment_DB|accountNumber: 8337100240261185"
let authline = "2025-08-25T07:00:56,737|4745BB4ED85C393E89D328AAB47ACB76|MOD25.09.0.004-DEBUG|decision.end0240_CheckAuthenticationStart_DS|authenticationEligible:[true],voiceBioEnrolled:[no]"
let catcodeline = "2025-08-25T09:58:47,902|1176F1F498AA1DE50F8030B588BA7739|MOD25.09.0.004-DEBUG|audio.end0305_PlayTransferMessage_PP|categoryCode: 010"
let initcfgline = "2025-08-25T05:17:55,485||MOD25.09.0.004-DEBUG|ivr.ConfigurationAccessor|initConfiguration: callerIntentConfigPath : /usr/local/shared/nuance-mod-v25-09-0_qa_ncw_app-1/nuance/external_config/nuancemoddockerconfig/caller_intent_config/qa//"
let invstartline = "2025-08-25T13:01:26,927||MOD25.09.0.004-WARN |calllog.InvocationCounter|InvocationCounter.valueUnbound: callstart was called [0]  times but it should have been called exactly once"
let invendline = "2025-08-25T13:00:26,917||MOD25.09.0.004-WARN |calllog.InvocationCounter|InvocationCounter.valueUnbound: callend was called [0]  times but it should have been called exactly once"
let catcodessalesline = "2025-08-25T10:59:27,228|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|audio.end0305_PlayTransferMessage_PP|catCodesForSalesTransfer: 015,021,028,079,080,201,202,203,204,205,293,120,121"
let playxferline = "2025-08-25T10:59:27,228|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-INFO |audio.end0305_PlayTransferMessage_PP|routingCode[Default] lastVisitedModule[portal] transferMsgPlayedFlag[false] portalName[Generic] identifiedFlag[false] delinquientLevel[] genericInterceptFOFlag[false] retryZipCode[false] botEligible[false] oofFlag[false] isPlayFreeSpecmoNotice[false] callType[Residential] preAuthLastModule[]"

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
}
