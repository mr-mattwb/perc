{
    open Unix
    open Printf
    open Stdlib

    type priority = 
        | DEBUG
        | INFO
        | WARN
        | ERROR

    module PrioSer =
        struct
            type elt = priority
            let of_string s = 
                match String.uppercase_ascii s with
                | "DEBUG" -> DEBUG
                | "INFO" -> INFO
                | "WARN" -> WARN
                | "ERROR" -> ERROR
                | _ -> raise (Failure ("invalid priority ["^s^"]"))
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

    module PMap = Map.Make(Int)
    type portalMap = string PMap.t

    module CatCodeTable = Map.Make(String)

    type position = 
        | Start
        | End

    type direction = 
        | Enter
        | Exit

    type init_config = {
        name : string;
        path : string
    }

    module BusinessUnitTable = Map.Make(String)
    
    type field_file = {
        field : string;
        file : string
    }

    type db_config_flag = {
        configId : string;
        fileName : string
    }

    type call_info = {
        dnis : string;
        ani : string;
        ucid : string;
        firstHistoryInfoUser : string;
        lastHistoryInfoUser : string;
        receivedUcid : string;
        receivedUui : string
    }

    type business_unit = {
        siteID : string;
        businessUnit : string;
        returnCode : string
    }

    type customerType = R | B | M | C
    type callType = 
        | Residential
        | Commercial
        | Business

    type nii_call_info = {
        portalName : string;
        regionById : string;
        poc : string;
        regionByDnis : string;
        isFromTargus : bool;
        callType : callType;
        isIdentifiedDegraded : bool;
        appTag : string;
        customerType : customerType;
        version : string;
        aniMatch : bool;
        endStatus : string;
        isIdentified : bool;
        unid : string
    }

    type base_cable_account = {
        accountNumber : string;
        accountMatchIdentifier : string;
        accountMatchSource : string;
        accountStatus : char;
        aniType : string option;
        billingLevel1 : int option;
        billingLevel2 : int option;
        billingLevel3 : int option;
        billingLevel4 : int option;
        businessUnit : string;
        connectDate : Date.t;
        customerType : customerType;
        firstName : string;
        lastName : string;
        cableSrvFlag : bool;
        hsiSrvFlag : bool;
        phoneServiceFlag : bool;
        seasonalDisconnected : bool;
        streetAddress : string;
        siteId : string;
        ssn : string;
        zip : string
    }

    type profile_info = {
        ucid : string;
        accountNumber : string;
        businessUnit : string;
        siteId : string;
        mobileAccountNumber : string;
        dnis : string
    }

    type service_affected = {
        sentenceId : string;
        collectionValue : string option;
        dmResult : string;
        dtmfOnly : bool;
        returnValue : string;
        agentRequested : bool;
        lastVisitedModule : string
    }

    type profile_status = {
        accountNumber : string;
        accountStatus : char 
    }

    type authentication_start = {
        eligible : bool;
        voiceBioEnrolled : bool
    }

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
        | CatCodesForSales of int list
        | ANILookup of string
        | PortalMap of portalMap
        | CatCodeTable of int CatCodeTable.t
        | Invocation of position
        | Visited of string * bool
        | MethodExec of string * direction
        | InitConfig of init_config
        | BusinessUnitTable of string BusinessUnitTable.t
        | NullFieldFile of field_file
        | LocalRepository of string
        | DBConfigFlag of db_config_flag
        | DBExec of string
        | CallInfo of call_info
        | BusinessUnitInfo of business_unit
        | NIICallInfo of nii_call_info
        | NodeHostname of string
        | BaseCableAccount of base_cable_account
        | ProfileInfo of profile_info
        | Language of string
        | PortalName of string
        | ResponseCode of int
        | LastVisitedModule of string
        | AppTag of string
        | ServiceAffected of service_affected
        | ProfileStatus of profile_status
        | AuthenticationStart of authentication_start
        | CreatedToken of string 
        | IpAddress of string
        | SecondaryHostUrl of string
        | Other of string

    let bool_of_string str = 
        match String.uppercase_ascii str with
        | "TRUE"|"T"|"1"|"YES"|"Y" -> true
        | "FALSE"|"F"|"0"|"NO"|"N" -> false
        | _ -> false

    let dir_of_string str = 
        match String.uppercase_ascii str with
        | "ENTER" -> Enter
        | "EXIT" -> Exit
        | _ -> Exit

    let callType_of_string s =
        match String.uppercase_ascii s with
        | "RESIDENTIAL" -> Residential
        | "COMMERCIAL" -> Commercial
        | "BUSINESS" -> Business
        | "" -> Residential
        | _ -> raise (Failure ("Invalid callType ["^s^"]"))
    let customerType_of_string s =
        match String.uppercase_ascii s with
        | "R" -> R
        | "B" -> B
        | "M" -> M
        | "C" -> C
        | _ -> R
    let customerType_of_char s = 
        match s with
        | 'R' | 'r' -> R
        | 'B' | 'b' -> B
        | 'M' | 'm' -> M
        | 'C' | 'c' -> C
        | _ -> R
}


let dig = ['0'-'9']

let year = ['1'-'2']['0'-'9']['0'-'9']['0'-'9']
let month = '0'['1'-'9']|'1'['0'-'2']
let day = '0'['1'-'9']|'3'['0'-'1']|['1'-'2']['0'-'9']

let hour = ['0'-'1']['0'-'9']|'2'['0'-'3']
let minute = ['0'-'5']['0'-'9']
let second = ['0'-'5']['0'-'9']

let msec = ['0'-'9']['0'-'9']['0'-'9']
let ident = ['0'-'9' 'A'-'F']
let version = ['0'-'9' 'A'-'Z' '.']
let priority = ("DEBUG"|"INFO"|"WARN"|"ERROR")
let func = [^ '|']

let itemTerm = [^ ',' '}']
let boolval = ("true"|"false"|"yes"|"no")
let allCapsDig = ['A'-'Z' '0'-'9']
let hexdig = ['A'-'F' '0'-'9']
let allCaps_Dig = ['A'-'Z' '_' '0'-'9']
let callType = "Residential"|"Commercial"|"Business"|""
let alpha_Dig = ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']
let customerType = "R"|"B"|"M"|"C"
let allCaps = ['A'-'Z']
let allCapsOpt = (['A'-'Z']+|"null")
let intOpt = (['0'-'9']+|"null")
let datetime = [ '0'-'9' '-' 'T' ':' ]
let allCapsDigSpc = ['A'-'Z' '0'-'9' ' ']
let digDash = ['0'-'9' '-']
let alphaDash = ['A'-'Z' 'a'-'z' '-']
let alphaDig = ['A'-'Z' 'a'-'z' '0'-'9']
let alphaDigNull = (['A'-'Z' 'a'-'z' '0'-'9']*|"null")
let url = ['A'-'Z' 'a'-'z' '0'-'9' '/' ':' '.' '?']

rule entry = parse
    | (year as yr) '-' (month as mo) '-' (day as da) 'T'
      (hour as hr) ':' (minute as mi) ':' (second as se) ','
      (msec as ms) '|' (ident* as id) '|' (version+ as vers) '-'
      (priority as prio) [' ']* '|' (func* as func) '|' 
      (_+ as dt) {
        {
            date = Date.of_strs yr mo da;
            time = Time.of_strs hr mi se;
            msec = int_of_string ms;
            iden = id;
            vers = vers;
            prio = PrioSer.of_string prio;
            func = func;
            data = data (Lexing.from_string dt) 
        }
    }
and data = parse
    | "chaining from >" (_+ as nfrom) "< to >" (_* as nto) "<" {
        Link {
            link_from = nfrom;
            link_to = nto
        }
    }
    | "Label: " (_+ as label) {
        Label label
    }
    | "catCodesForSalesTransfer: " {
        CatCodesForSales (catCodesForSales lexbuf)
    }
    | "[" (ident+ as id) "] Performing ANI Lookup" {
        ANILookup id
    }
    | "PortalMap[" (_* as pmap) "]" {
        PortalMap (portalMap (Lexing.from_string pmap))
    }
    | "categoryCodeTable={" (_* as ccTable) "}" {
        CatCodeTable (catCodeTable (Lexing.from_string ccTable))
    }
    | "InvocationCounter.valueUnbound: call" ("start"|"end" as pos) " was called [0]  times but it should have been called exactly once" {
        Invocation (match pos with "start" -> Start | _ -> End)
    }
    | "isVisitedNode(" ([^')']+ as fc) "): " (boolval as vn) {
        Visited(fc, bool_of_string vn)
    }
    | (_+ as fn) ": " ("Enter"|"Exit" as dir) "ing " ['m' 'M'] "ethod." {
        MethodExec (fn, dir_of_string dir)
    }
    | "initConfiguration: " (_+ as name) " : " (_+ as path) {
        InitConfig {
            name = name;
            path = path
        }
    }
    | "businessUnitTable={" (_+ as tbl) "}" {
        BusinessUnitTable (businessUnitTable (Lexing.from_string tbl))
    }
    | "Null value for " (_+ as field) " in fileName: " (_+ as fname) {
        NullFieldFile {
            field = field;
            file = fname
        }
    }
    | "loading configuration from local file repository: " (_+ as fname) {
        LocalRepository fname
    }
    | "DB Config Flag Map is Empty for Config ID: " (allCapsDig+ as cfgid) 
        " with fileName: " (_+ as fname) {
        DBConfigFlag {
            configId = cfgid;
            fileName = fname
        }
    }
    | "executing >" (_+ as sql) "<" {
        DBExec sql
    }
    | "DNIS :" (dig+ as dnis) " ANI :" (dig+ as ani)
      " UCID :" (hexdig+ as ucid) " FIRSTHISTORYINFOUSER :" (dig+ as fhiu)
      " LASTHISTORYINFOUSER :" (dig+ as lhiu) " RECEIVED_UCID :" (hexdig+|"null" as rucid)
      " RECEIVED_UUI :" (hexdig+|"null" as ruui) {
          CallInfo {
            dnis = dnis;
            ani = ani;
            ucid = ucid;
            firstHistoryInfoUser = fhiu;
            lastHistoryInfoUser = lhiu;
            receivedUcid = (match rucid with "null" -> "" | x -> x);
            receivedUui = (match ruui with "null" -> "" | x -> x)
          }
      }
    | "BusinessUnitInfo value: BusinessUnitInfo [siteID=" (dig+ as sid)
      ", businessUnit=" (allCapsDig+ as bu) ", returnCode=" (_+ as rc) "]" {
            BusinessUnitInfo {
                siteID = sid;
                businessUnit = bu;
                returnCode = rc
            }
      }
    | "CALL_INFO:PortalName=" (_+ as pn) "|RegionByID=" (_* as rid) "|POC=" (_* as poc)
        "|RegionByDNIS=" (allCaps_Dig* as regdnis) "|IsFromTargus=" (boolval as ifTargus) "|callType=" (callType as callt)
        "|IsIdentifiedDegraded=" (boolval as isid) "|AppTag=" (alpha_Dig* as appt) "|customerType=" (customerType* as cust) 
        "|Version=" (version+ as vers) "-0" "|AniMatch=" (boolval as anim) "|EndStatus=" (allCaps_Dig* as ends)
        "|IsIdentified=" (boolval as isident) "|Unid=" (_* as unid) {
            NIICallInfo {
                portalName = pn;
                regionById = rid;
                poc = poc;
                regionByDnis = regdnis;
                isFromTargus = bool_of_string ifTargus;
                callType = callType_of_string callt;
                isIdentifiedDegraded = bool_of_string isid;
                appTag = appt;
                customerType = customerType_of_string cust;
                version = vers;
                aniMatch = bool_of_string anim;
                endStatus = ends;
                isIdentified = bool_of_string isident;
                unid = unid
            }
        }
    | "NODE_HOSTNAME [" (_* as host) "]" {
            NodeHostname host
        }
    | " baseCableAccountIdx=0 AccountNumber: [" (dig+ as acctno) "] , AccountMatchIdentifier: [" (allCaps+ as amid)
        "] , AccountMatchSource: [" (allCaps+ as ams) "] , AccountStatus: [" (allCaps as accts) "] , ANIType: [" (allCapsOpt as anit) 
        "] , BillingLevel1: [" (intOpt as bl1) "] , BillingLevel2: [" (intOpt as bl2) "] , BillingLevel3: [" (intOpt as bl3) 
        "] , BillingLevel4: [" (intOpt as bl4) "] , BusinessUnit: [" (allCapsDig+ as bu) "] , ConnectDate: [" (datetime+ as cdate)
        "] , CustomerType: [" (customerType* as cust) "] , FirstName: [" (allCaps+ as fn) "] , LastName: [" (allCaps* as ln)
        "] , CableSrvFlag: [" (boolval as csf) "] , HSISrvFlag: [" (boolval as hsisf) "] , PhoneServiceFlag: [" (boolval as psf)
        "] , SeasonalDisconnected: [" (boolval as sd) "] , StreetAddress: [" (allCapsDigSpc+ as sa) "] , SiteId: [" (dig+ as sid)
        "] , SocialSecurity: [" (digDash* as ssn) "] , Zip: [" (dig+ as zip) {
            BaseCableAccount {
                accountNumber = acctno;
                accountMatchIdentifier = amid;
                accountMatchSource = ams;
                accountStatus = accts;
                aniType = (match anit with "null" -> None | v -> Some v);
                billingLevel1 = (match bl1 with "null" -> None | v -> Some (int_of_string bl1));
                billingLevel2 = (match bl2 with "null" -> None | v -> Some (int_of_string bl2));
                billingLevel3 = (match bl3 with "null" -> None | v -> Some (int_of_string bl3));
                billingLevel4 = (match bl4 with "null" -> None | v -> Some (int_of_string bl4));
                businessUnit = bu;
                connectDate = date (Lexing.from_string cdate);
                customerType = customerType_of_string cust;
                firstName = fn;
                lastName = ln;
                cableSrvFlag = bool_of_string csf;
                hsiSrvFlag = bool_of_string hsisf;
                phoneServiceFlag = bool_of_string psf;
                seasonalDisconnected = bool_of_string sd;
                streetAddress = sa;
                siteId = sid;
                ssn = ssn;
                zip = zip
            }
    }
    | "Executing getProfileInfoUsingGET ucid[" (hexdig+ as ucid) "] accountNumber[" (dig+ as acctno) 
        "] businessUnit[" (allCaps* as bu) "] siteId[" (dig* as sid) "] mobileAccountNumber[" (dig* as man)
        "] dnis[" (dig* as dnis) "]" {
            ProfileInfo {
                ucid = ucid;
                accountNumber = acctno;
                businessUnit = bu;
                siteId = sid;
                mobileAccountNumber = man;
                dnis = dnis
            }
        }
    | "language:[" (alphaDash+ as lang) "]" {
            Language lang 
    }
    | "portalName: [" (alphaDig+ as pn) "]" {
            PortalName pn
    }
    | "responseCode: " (dig+ as rc) {
            ResponseCode (int_of_string rc)
    }
    | "lastVisitedModule" [' ']* ": " '['? (alphaDig+ as lvm) ']'? {
            LastVisitedModule lvm
    }
    | "appTag : " (alphaDig+ as appt) {
            AppTag appt
    }
    | "sentenceId : [" (alpha_Dig* as sid) "] collectionValue : [" (alphaDigNull as collval) "] "
      "outage0240_CollectServiceAffected_DM_Result : [" (alphaDig* as o0240) "] dtmfOnly : [" (boolval as dtmfOnly) 
      "] returnValue : [" (alpha_Dig* as retVal) "] agentRequested : [" (boolval as agentReq) 
      "]  lastVisitedModule : [" (alpha_Dig* as lvm) " ]" {
            ServiceAffected {
                sentenceId = sid;
                collectionValue = (match collval with "null" -> None | v -> Some v);
                dmResult = o0240;
                dtmfOnly = bool_of_string dtmfOnly;
                returnValue = retVal;
                agentRequested = bool_of_string agentReq;
                lastVisitedModule = lvm
            }
      }
    | "intent0110_Routing_DS: cableProfile (accountNumber[" (dig* as acctno) "] accountStatus[" (alphaDig as acctst) "])" {
        ProfileStatus {
            accountNumber = acctno;
            accountStatus = acctst
        }
    }
    | "authenticationEligible:[" (boolval as elig) "],voiceBioEnrolled:[" (boolval as enrolled) "]" {
        AuthenticationStart {
            eligible = bool_of_string elig;
            voiceBioEnrolled = bool_of_string enrolled
        }
    }
    | "Created _TOKEN [" (alpha_Dig+ as tok) "]" {
        CreatedToken tok
    }
    | "execute: found ipAddress: " (alphaDig+ as ipaddr) {
        IpAddress ipaddr
    }
    | "FreeSpeech client secondaryHostUrl successfully created: " (url+ as url) {
        SecondaryHostUrl url
    }

and businessUnitTable = parse
    | [' ']* ',' [' ']*                             { businessUnitTable lexbuf }
    | eof                                           { BusinessUnitTable.empty }
    | (allCapsDig+ as code) '=' (allCapsDig+ as bu)       {
        BusinessUnitTable.add code bu (businessUnitTable lexbuf)
    }
and catCodeTable = parse
    | [' ']* ',' [' ']*                             { catCodeTable lexbuf }
    | eof                                           { CatCodeTable.empty }
    | ([^'=' ',' ' ']+ as cat) '=' (dig+ as code)   {
        CatCodeTable.add cat (int_of_string code) (catCodeTable lexbuf) }
and portalMap = parse
    | '{'                               { portalMap lexbuf }
    | '}'                               { PMap.empty }
    | [' ']* ',' [' ']*                 { portalMap lexbuf }
    | (dig+ as mc) '=' (itemTerm+ as pmap)    {
        PMap.add (int_of_string mc) pmap (portalMap lexbuf)
    }
and catCodesForSales = parse
    | ','                               { catCodesForSales lexbuf }
    | eof                               { [] }
    | (dig+ as code)                    { (int_of_string code) :: (catCodesForSales lexbuf) }

and date = parse
    | (year as yr) '-' (month as mo) '-' (day as dy) { Date.of_strs yr mo dy }

and time = parse
    | (hour as hr) ':' (minute as mi) ':' (second as se) { Time.of_strs hr mi se }

{
    let parse_entry str = 
        try
            Some (entry (Lexing.from_string str))
        with Failure _ ->
            None

    let parse_channel fin = 
        let rec aux ls = 
            match Tools.input_line fin with
            | None -> ls
            | Some line -> aux (add_entry line ls)
        and add_entry line ls = 
            match parse_entry line with
            | None -> aux ls
            | Some e -> aux (e :: ls)
        in
        aux []
    let input_entries fname = Tools.with_in_file parse_channel fname 
}
