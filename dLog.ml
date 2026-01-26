open Unix
open Printf
open Stdlib

type priority = 
    | Debug
    | Info
    | Warn
    | Error

type dir =
    | Start
    | End

module PortalMap = Map.Make(Int)
module CatCodeTable = Map.Make(String)
module BusinessUnitTable = Map.Make(String)

type header = {
    date : Date.t;
    time : Time.t;
    msec : int;
    iden : string;
    vers : string;
    prio : priority;
    func : string
}
and data = 
    | Link of link
    | Label of string
    | Invocation of dir
    | State of string
    | NextModule of string
    | ReturnFunction of string
    | LastVisitedModule of string
    | PortalName of string
    | ReturnValue of string
    | NodeHostname of string
    | CallInfo of call_info
    | Language of string
    | CatCodesForSalesTransfer of int list
    | FindCustomerUsingGET of find_customer_using_get
    | CreatedToken of string
    | PortalMap of portalmap
    | VisitedNode of string * bool
    | CatCodeTable of catcode_table
    | SqlExecuting of string
    | NullItemPath of string * string
    | EmptyCallflowMap of string * string
    | LoadLocalConfig of string
    | AppTag of string
    | IpAddress of string
    | BusinessUnitTable of business_unit_table
    | Other of string
and link = {
    link_from : string;
    link_to : string
}
and call_info = {
    dnis : string;
    ani : string;
    ucid : string;
    firstHistoryInfoUser : string;
    lastHistoryInfoUser : string;
    receivedUcid : string;
    receivedUui : string
}
and find_customer_using_get = {
    ucid : string;
    ani : string;
    ced : string;
    acct : string;
    dnis : string;
    siteId : string
}
and portalmap = string PortalMap.t
and catcode_table = int CatCodeTable.t
and business_unit_table = string BusinessUnitTable.t

