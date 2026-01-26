{
    open Unix
    open Printf
    open Stdlib

    open DLog
    open DParse

    let priority_of_string = function
        | "DEBUG" -> Debug
        | "INFO" -> Info
        | "WARN" -> Warn
        | "ERROR" -> Error
        | p -> raise (Failure ("Invalid priority ["^p^"]")) 
    let dir_of_string = function
        | "start" -> Start
        | "end" -> End
        | p -> raise (Failure ("Invalid direction ["^p^"]"))
}
let dig = ['0'-'9']
let year = ['1''2'] dig dig dig
let month = '0'['1'-'9']|'1'['0'-'2']
let day = '0'['1'-'9']|['1''2'] dig|'3'['0''1']
let hour = ['0''1'] dig | '2' ['0'-'3']
let minute = ['0'-'5'] dig
let second = ['0'-'5'] dig
let ucHex = ['0'-'9' 'A'-'F']
let version = [^'-']
let priority = "DEBUG"|"INFO"|"WARN"|"ERROR"
let notbar = [^'|']
let nobrack = [^']']
let nobracks = nobrack*
let hexnull = ['0'-'9' 'A'-'F']*|"null"
let portalName = ['A'-'Z' '0'-'9' '-' '_']
let boolval = "true"|"false"
let alphaDig = ['A'-'z' '0'-'9' '_' '-']
let comma = [' ']*','[' ']*

rule header = parse
    | (year as yr) '-' (month as mo) '-' (day as da)            { DATE (Date.of_strs yr mo da) }
    | 'T' (hour as hr) ':' (minute as mi) ':' (second as se)    { TIME (Time.of_strs hr mi se) }
    | ',' (dig dig dig as ms)                                   { MSEC (int_of_string ms) }
    | '|' (ucHex* as id) '|' (version+ as ver)                  { IDEN (id, ver) }
    | '-' (priority as prio) [' ']* '|' (notbar+ as func) '|'   { PRIO (priority_of_string prio, func) }

and data = parse
    | "chaining from >"([^'<']+ as nfrom)
      "< to >"([^'<']+ as nto)"<"                               { LINK { link_from=nfrom; link_to=nto } }
    | "Label: " (_+ as label)                                   { LABEL label }
    | "InvocationCounter.valueUnbound: call" ("start"|"end" as dir) " was called [0]  times but it should have been called exactly once" {
            INVOCATION (dir_of_string dir)
        }
    | ([^':']+ as func)": entering state"                        { STATE func }
    | "nextModule: ["(nobrack+ as nmod)"]"                       { NEXTMODULE nmod }
    | "Return["(nobrack+ as ret)"]"                              { RETURNFUNCTION ret }
    | "lastVisitedModule" [' ']*':'[' ']* '['? (nobrack+ as lvm) ']'? { LASTVISITEDMODULE lvm }
    | "portalName: ["(nobrack* as pn)"]"                           { PORTALNAME pn }
    | "returnValue:["(nobrack* as rv)"]"                         { RETURNVALUE rv }
    | "NODE_HOSTNAME ["(nobrack* as nh)"]"                       { NODEHOSTNAME nh } 
    | "DNIS :"(dig+ as dnis)" ANI :"(dig+ as ani)" UCID :"(ucHex+ as ucid)" FIRSTHISTORYINFOUSER :"(dig+ as fhiu)" LASTHISTORYINFOUSER :"(dig+ as lhiu)
        " RECEIVED_UCID :"(hexnull as rucid)" RECEIVED_UUI :"(hexnull as ruui) {
            CALLINFO {
                dnis = dnis;
                ani = ani;
                ucid = ucid;
                firstHistoryInfoUser = fhiu;
                lastHistoryInfoUser = lhiu;
                receivedUcid = (match rucid with "null" -> "" | s -> s);
                receivedUui = (match ruui with "null" -> "" | s -> s)
            }
        }
    | "language:["(nobrack* as lang)"]"                          { LANGUAGE lang }
    | "Executing findCustomerUsingGET ucid["(nobrack* as ucid)"] ani["(nobrack* as ani)"] ced["(nobrack* as ced)"] accountNumber["(nobrack* as acctno)
        "] dnis["(nobrack* as dnis)"] siteId["(nobrack* as sid)"]" {
            FINDCUSTOMERUSINGGET {
                ucid = ucid;
                ani = ani;
                ced = ced;
                acct = acctno;
                dnis = dnis;
                siteId = sid
            }
        }
    | "catCodesForSalesTransfer: "                                  { CATCODESFORSALESTRANSFER (catCodesForSalesTransfer lexbuf) }
    | "Created _TOKEN ["(nobrack+ as tkn)"]"                        { CREATEDTOKEN tkn }
    | "PortalMap[{"                                                 { PORTALMAP (portalMap lexbuf) }
    | "isVisitedNode("([^')']+ as node)"): "(boolval as visited)    { VISITEDNODE (node, bool_of_string visited) }
    | "categoryCoeTable={"                                          { CATCODETABLE (catCodeTable lexbuf) }
    | "executing >" (_* as sql) "<"                                 { SQLEXECUTING sql }
    | "Null value for "([^' ']+ as name)" in fileName: "(_* as path)    { NULLITEMPATH(name, path)  }
    | "DB Config Flag Map is Empty for Config ID: "([^' ']+ as cf)" with fileName: "(_+ as fn) { EMPTYCALLFLOWMAP(cf, fn) }
    | "loading configuration from local file repository: "(_+ as path) { LOADLOCALCONFIG path }
    | "appTag : "(_+ as apptag)                                     { APPTAG apptag }
    | "execute: found ipAddress: "(_+ as ip)                        { IPADDRESS ip }
    | "businessUnitTable={"                                         { BUSINESSUNITTABLE (businessUnitTable lexbuf) }

and businessUnitTable = parse
    | "}"                                                           { BusinessUnitTable.empty }
    | comma                                                         { businessUnitTable lexbuf }
    | (alphaDig+ as num)'='(alphaDig+ as name)                      { BusinessUnitTable.add num name (businessUnitTable lexbuf) }
and catCodeTable = parse
    | "}"                                                           { CatCodeTable.empty }
    | comma                                                         { catCodeTable lexbuf }
    | (alphaDig+ as name)'='(dig+ as code)                          { CatCodeTable.add name (int_of_string code) (catCodeTable lexbuf) }
and portalMap = parse
    | "}]"                                                          { PortalMap.empty }
    | comma                                                         { portalMap lexbuf }
    | (dig+ as num)'='(portalName+ as name)                         { PortalMap.add (int_of_string num) name (portalMap lexbuf) }
and catCodesForSalesTransfer = parse
    | ","                                                           { catCodesForSalesTransfer lexbuf }
    | eof                                                           { [] }
    | (dig+ as cc)                                                  { (int_of_string cc) :: (catCodesForSalesTransfer lexbuf) }


{
let parse_entry line = 
    try
        let lex = Lexing.from_string line in
        let hdr = DParse.header header lex in
        let data = DParse.data data lex in
        Some (hdr, data)
    with e ->
        None

let parse_channel fin = 
    let rec aux ls = 
        match Tools.input_line fin with
        | None -> ls
        | Some line -> aux (apply ls line)
    and apply ls line = 
        match parse_entry line with
        | None -> ls
        | Some e -> e :: ls
    in
    aux []

let parse_file fname = Tools.with_in_file parse_channel fname 

}
