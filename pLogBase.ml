open Unix
open Printf
open Stdlib

type id = string

type 'a entry = {
    date : Date.t;
    time : Time.t;
    msec : int;
    id : id;
    ivr : string;
    prio : string;
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

let of_strs (yr, mn, dy) (hh, mm, ss) ms id ver pri func data = {
    date = Date.of_strs yr mn dy;
    time = Time.of_strs hh mm ss;
    msec = int_of_string ms;
    id = id;
    ivr = ver;
    prio = pri;
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

let baseline = "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true"
let linkline = "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<"
let labelline = "2025-08-25T05:28:20,289|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: dummy_dm"
let labelline2 = "2025-08-25T05:24:35,164|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-INFO |decision.languageSwitchOB|Exiting method. Label: Switch_English_OB"
let stateline = "2025-08-25T05:28:54,469|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state"

let fakelinkline = "2025-08-25T05:17:55,457||MOD25.09.0.004-INFO |ivr.ConfigurationAccessor|ConfigurationAccessor.initConfiguration: Entering method."

