{
open Unix
open Printf
open Stdlib

module Ser = 
    struct
        class type ['a] elt = 
            object
                method of_str : string -> 'a
                method to_str : 'a -> string
            end
        let str_ser : string elt = 
            object
                method of_str s = s
                method to_str s = s
            end
        let int_ser : int elt = 
            object
                method of_str = int_of_string
                method to_str = string_of_int
            end
        let flt_ser : float elt =
            object
                method of_str = float_of_string
                method to_str = string_of_float
            end
        let bool_ser : bool elt =
            object
                method of_str = bool_of_string
                method to_str = string_of_bool
            end
        let int32_ser : int32 elt =
            object
                method of_str = Int32.of_string
                method to_str = Int32.to_string
            end
        let int64_ser : int64 elt =
            object
                method of_str = Int64.of_string
                method to_str = Int64.to_string
            end
    end

module Env = 
    struct
        class type ['a] var = 
            object
                method name : string
                method default : 'a
                method descr : string
                method switches : string list
            end
        class type ['a] elt =
            object
                method ser : 'a Ser.elt
                method var : 'a var
                method get : unit -> 'a
                method set : 'a -> unit
            end
        let new_var (s : 'a Ser.elt) (v : 'a var) = 
            object (self)
                method ser = s
                method var = v
                method get () =
                    try self#ser#of_str (Unix.getenv self#var#name)
                    with _ -> self#set self#var#default; self#get()
                method set v = 
                    Unix.putenv self#var#name (self#ser#to_str v)
            end
        let str_var (n : string var) : string elt = new_var Ser.str_ser n
        let int_var n : int elt = new_var Ser.int_ser n
        let int32_var n : int32 elt = new_var Ser.int32_ser n
        let int64_var n : int64 elt = new_var Ser.int64_ser n
        let flt_var n : float elt = new_var Ser.flt_ser n
        let bool_var n : bool elt = new_var Ser.bool_ser n
    end

module Log = 
    struct
        type priority = 
            | Off
            | Debug
            | Info
            | Warn
            | Error
        type chan = 
            | File of Tools.file
            | Chan of out_channel
        let priority_ser = 
            object 
                method to_str = function
                    | Off -> "OFF"
                    | Debug -> "DEBUG"
                    | Info -> "INFO"
                    | Warn -> "WARN"
                    | Error -> "ERROR"
                method of_str s = 
                    match String.uppercase_ascii s with
                    | "OFF" -> Off
                    | "DEBUG" -> Debug
                    | "INFO" -> Info
                    | "WARN" -> Warn
                    | "ERROR" -> Error
                    | s -> raise (Failure ("Invalid priority ["^s^"]")) 
            end
        let chan_ser = 
            object
                method to_str = function
                    | File f -> f
                    | Chan c when c = Stdlib.stdout -> "CHANNEL:STDOUT"
                    | Chan c when c = Stdlib.stderr -> "CHANNEL:STDERR"
                    | Chan _ -> raise (Failure "Cannot convert channel to string ")
                method of_str = function
                    | "CHANNEL:STDOUT" -> Chan stdout
                    | "CHANNEL:STDERR" -> Chan stderr
                    | f -> File f
            end
        let priority_var = Env.new_var priority_ser 
        let chan_var = Env.new_var chan_ser 
        let message pri modn msg = 
            let tm = Unix.localtime (Unix.time()) in
            sprintf "%04d-%02d-%02d %02d:%02d:%02d %8d [%s] [%s] %s"
                (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday
                tm.tm_hour tm.tm_min tm.tm_sec 
                (Unix.getpid()) (priority_ser#to_str pri) modn msg
        let priority_env = priority_var 
            object
                method name = "LOG_PRIORITY"
                method default = Debug
                method descr = "Log priority"
                method switches = [ "--log-priority" ]
            end
        let chan_env = chan_var 
            object
                method name = "LOG_CHANNEL"
                method default = Chan stderr
                method descr = "Log channel"
                method switches = [ "--log-channel" ]
            end
        let modName_env = Env.str_var
            object
                method name = "LOG_MODULE"
                method default = Tools.basename
                method descr = "Default log name"
                method switches = [ "--log-module" ]
            end

        class type elt = 
            object
                method chan : unit -> chan
                method priority : unit -> priority
                method modName : unit -> string
            end
        class log (ch : chan) (pri : priority) (modn : string) = 
            object
                method chan () = ch
                method priority () = pri
                method modName () = modn
            end
        class env_log () = 
            object
                method chan () = chan_env#get()
                method priority () = priority_env#get()
                method modName () = modName_env#get()
            end
        class def_env_log modn = 
            object
                inherit env_log ()
                method modName () = modn
            end

        module type ELT = 
            sig
                val printf : elt -> ('a, unit, string, unit) format4 -> 'a
                val debug : elt -> ('a, unit, string, unit) format4 -> 'a
                val info : elt -> ('a, unit, string, unit) format4 -> 'a
                val warn : elt -> ('a, unit, string, unit) format4 -> 'a
                val error : elt -> ('a, unit, string, unit) format4 -> 'a
            end
            
        module Elt : ELT = 
            struct
                let write_msg lg msg pri fout =
                    output_string fout (message pri (lg#modName()) msg);
                    output_string fout "\n";
                    flush fout
                let write_chan (lg : elt) pri msg =
                    match lg#chan () with
                    | Chan fout -> write_msg lg msg pri fout
                    | File fname -> Tools.with_out_file (write_msg lg msg pri) fname
                let printf lg fmt = ksprintf (write_chan lg Off) fmt
                let debug (lg : elt) (fmt : ('a, unit, string, unit) format4) = 
                    if (lg#priority()) <> Off && (lg#priority()) = Debug then
                        ksprintf (write_chan lg Debug) fmt
                    else
                        let nothing _ = () in
                        ksprintf nothing fmt
                let info lg fmt =
                    if (lg#priority()) <> Off && ((lg#priority()) = Debug || (lg#priority()) = Info) then
                        ksprintf (write_chan lg Info) fmt
                    else
                        let nothing _ = () in
                        ksprintf nothing fmt
                let warn lg fmt = 
                    if (lg#priority()) <> Off && (lg#priority()) <> Error then
                        ksprintf (write_chan lg Warn) fmt
                    else
                        let nothing _ = () in
                        ksprintf nothing fmt
                let error lg fmt = 
                    if (lg#priority()) <> Off then
                        ksprintf (write_chan lg Error) fmt
                    else
                        let nothing _ = () in
                        ksprintf nothing fmt
            end
        include Elt
    end

module PortalMap = Map.Make(Int)
type portal = string
type portal_map = portal PortalMap.t

module BusinessUnitTable = Map.Make(String)
type business_unit_table = string BusinessUnitTable.t

module CategoryCodeTable = Map.Make(String)
type category_code_table = string CategoryCodeTable.t

}

rule token = parse
    | eof                                               { "","" }
    | [' ' '\t' ]*                                      { token lexbuf }
    | [^'=' '#' ' ' '\t']* as lhs                       { equals lhs lexbuf }
and equals lhs = parse
    | eof                                               { lhs, "" }
    | [' ' '\t' ]*                                      { equals lhs lexbuf }
    | '='                                               { righthandside lhs lexbuf }
and righthandside lhs = parse
    | eof                                               { lhs, "" }
    | [' ' '\t']*                                       { righthandside lhs lexbuf }
    | '#' _ eof                                         { righthandside lhs lexbuf }
    | '"' (([^'"']|"\\\"")* as rhs) '"'                 { lhs, String.trim (Pcre.replace ~pat:"\\\\\"" ~templ:"\"" rhs) }
    | [' ''\t']*([^'#' '"' ]* as rhs)[' ' '\t']*        { lhs, String.trim rhs }

and portalMapEntries = parse
    | eof                                               { PortalMap.empty }
    | [' ''\t']*','[' ''\t']*                            { portalMapEntries lexbuf }
    | ((['0'-'9']['0'-'9']) as key) '=' (['A'-'z' '0'-'9' '-' '_' ]* as portal) {
        PortalMap.add (int_of_string key) portal (portalMapEntries lexbuf)
    }
and businessUnitEntries = parse
    | eof                                               { BusinessUnitTable.empty }
    | [' ''\t']*','[' ''\t']*                           { businessUnitEntries lexbuf }
    | ([^'=']* as key) '=' ([^',']* as code)            { BusinessUnitTable.add key code (businessUnitEntries lexbuf) }

and categoryCodeEntries = parse
    | eof                                               { CategoryCodeTable.empty }
    | [' ''\t']*','[' ''\t']*                           { categoryCodeEntries lexbuf }
    | ([^'=']* as key) '=' ([^',']* as code)            { CategoryCodeTable.add key code (categoryCodeEntries lexbuf) }

{
module Config = 
    struct
        let parse_line line = 
            try 
                let key, value =  token (Lexing.from_string line) in
                Unix.putenv key value;
                Some (key, value)
            with _ -> 
                None
        let rec parse_channel fin = 
            match Tools.input_line fin with
            | None -> ()
            | Some line -> 
                ignore (parse_line line);
                parse_channel fin
        let parse_file fname = Tools.with_in_file parse_channel fname
        let file_env = Env.str_var
            object
                method name = "CONFIG_FILE"
                method default = Tools.basename^".cfg"
                method descr = "Configuration file name"
                method switches = [ "--cfg-file" ]
            end
        let parse_env () = parse_file (file_env#get())
    end

type prio = 
    | DEBUG
    | INFO 
    | WARN
    | ERROR


class type link =
    object
        method link_from : string
        method link_to : string
    end
 
type calldir = Start | End

class type call_info = 
    object
        method dnis : string
        method ani : string
        method ucid : string
        method firstHistoryInfoUser : string
        method lastHistoryInfoUser : string
        method receivedUcid : string
        method receivedUui : string
    end
class type service_url = 
    object
        method url : string
        method path : string
    end
class type db_config_flag = 
    object
        method configId : string
        method fileName : string
    end
class type init_configuration =
    object
        method path : string
        method fileName : string
    end
class type null_value = 
    object
        method nullValue : string
        method fileName : string
    end


type data =
    | Default of string
    | Label of string
    | Link of link
    | InvocationCounter of calldir
    | CallInfo of call_info
    | InitClient of string
    | ExecutingSQL of string
    | WebServiceURL of service_url
    | ClientServiceURL of service_url
    | PortalMap of portal_map
    | DBConfigFlag of db_config_flag
    | InitConfiguration of init_configuration
    | LoadConfiguration of string
    | BusinessUnitTable of business_unit_table
    | CategoryCodeTable of category_code_table
    | NullValue of null_value
    | InitServiceClient of string
    | ReadFromDBFinished of int
    | NoResourceMethods of string
    | CatCodesForSalesTransfer of int list
    | ReturnValue of string
    | IsVisited of string * bool

class type entry_t = 
    object
        method date : int
        method time : int
        method msec : int
        method iden : string
        method vers : string
        method prio : prio
        method func : string
        method data : data
    end

let year    = "([0-2][0-9][0-9][0-9])"
let month   = "(0[1-9]|1[0-2]])"
let day     = "(0[1-9]|[1-2][0-9]|3[0-1])"
let hour    = "([0-1][0-9]|2[0-3])"
let minute  = "([0-5][0-9])"
let second  = "([0-5][0-9])"
let msec    = "([0-9][0-9][0-9])"
let date = year^"-"^month^"-"^day
let time = hour^":"^minute^":"^second
let datetime = date^"T"^time
let datetime_msec = datetime^","^msec
let identifier = "([0-9A-F]*)"
let version = "([^\\-]+)"
let priority = "(DEBUG|INFO|WARN|ERROR)"
let funcName = "([^\\|]+)"
let entry = datetime_msec^"\\|"^identifier^"\\|"^version^"-"^priority^"[ ]*\\|"^funcName^"\\|(.*)"

let date_of_ints y m d = (y * 10000) + (m * 100) + d
let date_of_strs y m d = date_of_ints (int_of_string y) (int_of_string m) (int_of_string d)
let time_of_ints h m s = (h * 10000) + (m * 100) + s
let time_of_strs h m s = time_of_ints (int_of_string h) (int_of_string m) (int_of_string s)
let prio_of_string s = 
    match String.uppercase_ascii s with
    | "DEBUG" -> DEBUG
    | "INFO" -> INFO
    | "WARN" -> WARN
    | "ERROR" -> ERROR
    | v -> raise (Failure ("Invalid priority ["^v^"]"))

let calldir_of_string s =
   match String.uppercase_ascii s with
    | "START" -> Start 
    | "END" -> End
    | _ -> raise (Failure ("Invalid call direction ["^s^"]"))

let default_pat = "(.*)"
let label_pat = "^Label: (.*)"
let link_pat = "chaining from >(.*)< to >(.*)<"
let invocationCounter_pat = "InvocationCounter.valueUnbound: call(start|end) was called \\[0\\]  times but it should have been called exactly once"
let callInfo_pat = "DNIS :(.*) ANI :(.*) UCID :(.*) FIRSTHISTORYINFOUSER :(.*) LASTHISTORYINFOUSER :(.*) RECEIVED_UCID :(.*) RECEIVED_UUI :(.*)" 
let initClient_pat = "Initializing (.*) Restful [sS]ervice [cC]lient"
let executingSQL_pat = "^executing >(.*)<"
let webServiceURL_pat = "([^ ]URL)[ ]*: (.*)"
let clientServiceURL_pat = "(.*) Service URL: (.*)"
let portalMap_pat = "^PortalMap\\[{(.*)}\\]"
let dbConfigFlag_pat = "DB Config Flag Map is Empty for Config ID: (.*) with fileName: (.*)"
let initConfiguration_pat = "initConfiguration: (.*) : (.*)"
let loadConfiguration_pat = "loading configuration from local file repository: (.*)"
let businessUnitTable_pat = "businessUnitTable={(.*)}"
let categoryCodeTable_pat = "categoryCodeTable={(.*)}"
let nullValue_pat = "Null value for (.*) in fileName: (.*)"
let initServiceClient_pat = "Initializing (.*) [sS]ervice [cC]lient"
let readFromDBFinished_pat = "readFromDB finished. It took (.*) milliseconds to complete."
let noResourceMethods_pat = "No resource methods have been found for resource class (.*)"
let catCodesForSalesTransfer_pat = "^catCodesForSalesTransfer: (.*)"
let returnValue_pat = "^returnValue:\\[(.*)\\]"
let isVisited_pat = "^isVisitedNode\\((.*)\\): (.*)"

let parse1 pat line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat line) in
    ss.(1)
let parse_label line =
    let ss = Pcre.get_substrings (Pcre.exec ~pat:label_pat line) in
    Label ss.(1)
let parse_link line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:link_pat line) in
    Link (object method link_from = ss.(1) method link_to = ss.(2) end)
let parse_default line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:default_pat line) in
    Default ss.(1)
let parse_invocationCounter line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:invocationCounter_pat line) in
    InvocationCounter (calldir_of_string ss.(1))
let parse_callInfo line = 
    let null_str s =
        match String.uppercase_ascii s with
        | "NULL" -> ""
        | _ -> s
    in
    let ss = Pcre.get_substrings (Pcre.exec ~pat:callInfo_pat line) in
    CallInfo (object
        method dnis = ss.(1)
        method ani = ss.(2)
        method ucid = ss.(3)
        method firstHistoryInfoUser = ss.(4)
        method lastHistoryInfoUser = ss.(5)
        method receivedUcid = null_str ss.(6)
        method receivedUui = null_str ss.(7)
    end)
let parse_initClient line = InitClient (parse1 initClient_pat line)
let parse_executingSQL line = ExecutingSQL (parse1 executingSQL_pat line)
let parse_webServiceURL line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:webServiceURL_pat line) in
    WebServiceURL (
    object
        method url = ss.(1)
        method path = ss.(2)
    end)
let parse_clientServiceURL line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:clientServiceURL_pat line) in
    ClientServiceURL (
    object
        method url = ss.(1)
        method path = ss.(2)
    end)
let parse_portalMap line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:portalMap_pat line) in
    PortalMap (portalMapEntries (Lexing.from_string ss.(1))) 
let parse_dbConfigFlag line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:dbConfigFlag_pat line) in
    DBConfigFlag (
    object
        method configId = ss.(1)
        method fileName = ss.(2)
    end)
let parse_initConfiguration line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:initConfiguration_pat line) in
    InitConfiguration (
    object
        method path = ss.(1)
        method fileName = ss.(2)
    end) 
let parse_loadConfiguration line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:loadConfiguration_pat line) in
    LoadConfiguration ss.(1)
let parse_businessUnitTable line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:businessUnitTable_pat line) in
    BusinessUnitTable (businessUnitEntries (Lexing.from_string ss.(1)))
let parse_categoryCodeTable line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:categoryCodeTable_pat line) in
    CategoryCodeTable (categoryCodeEntries (Lexing.from_string ss.(1)))
let parse_nullValue line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:nullValue_pat line) in
    NullValue (
    object
        method nullValue = ss.(1)
        method fileName = ss.(2)
    end)
let parse_initServiceClient line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:initServiceClient_pat line) in
    InitServiceClient ss.(1)
let parse_readFromDBFinished line =
    let ss = Pcre.get_substrings (Pcre.exec ~pat:readFromDBFinished_pat line) in
    ReadFromDBFinished (int_of_string ss.(1))
let parse_noResourceMethods line =
    let ss = Pcre.get_substrings (Pcre.exec ~pat:noResourceMethods_pat line) in
    NoResourceMethods ss.(1)
let parse_catCodesForSalesTransfer line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:catCodesForSalesTransfer_pat line) in
    CatCodesForSalesTransfer (List.map int_of_string (Pcre.split ~pat:"," ss.(1)))
let parse_returnValue line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:returnValue_pat line) in 
    ReturnValue ss.(1)
let parse_isVisited line = 
    let ss = Pcre.get_substrings (Pcre.exec ~pat:isVisited_pat line) in
    IsVisited (ss.(1), bool_of_string ss.(2))

exception Unsupported_node of string

let new_data line = 
    match line with
    | line when Pcre.pmatch ~pat:label_pat line -> parse_label line
    | line when Pcre.pmatch ~pat:link_pat line -> parse_link line
    | line when Pcre.pmatch ~pat:invocationCounter_pat line -> parse_invocationCounter line
    | line when Pcre.pmatch ~pat:callInfo_pat line -> parse_callInfo line
    | line when Pcre.pmatch ~pat:initClient_pat line -> parse_initClient line
    | line when Pcre.pmatch ~pat:executingSQL_pat line -> parse_executingSQL line
    | line when Pcre.pmatch ~pat:webServiceURL_pat line -> parse_webServiceURL line
    | line when Pcre.pmatch ~pat:clientServiceURL_pat line -> parse_clientServiceURL line
    | line when Pcre.pmatch ~pat:portalMap_pat line -> parse_portalMap line
    | line when Pcre.pmatch ~pat:dbConfigFlag_pat line -> parse_dbConfigFlag line 
    | line when Pcre.pmatch ~pat:businessUnitTable_pat line -> parse_businessUnitTable line
    | line when Pcre.pmatch ~pat:categoryCodeTable_pat line -> parse_categoryCodeTable line
    | line when Pcre.pmatch ~pat:nullValue_pat line -> parse_nullValue line
    | line when Pcre.pmatch ~pat:initServiceClient_pat line -> parse_initServiceClient line
    | line when Pcre.pmatch ~pat:readFromDBFinished_pat line -> parse_readFromDBFinished line
    | line when Pcre.pmatch ~pat:noResourceMethods_pat line -> parse_noResourceMethods line
    | line when Pcre.pmatch ~pat:catCodesForSalesTransfer_pat line -> parse_catCodesForSalesTransfer line
    | line when Pcre.pmatch ~pat:returnValue_pat line -> parse_returnValue line
    | line when Pcre.pmatch ~pat:isVisited_pat line -> parse_isVisited line
    | _ -> raise (Unsupported_node line)

let parse_entry line : entry_t =
        let ss = Pcre.get_substrings (Pcre.exec ~pat:entry line) in
        let data = new_data ss.(12) in
        object
            method date = date_of_strs ss.(1) ss.(2) ss.(3); 
            method time = time_of_strs ss.(4) ss.(5) ss.(6);
            method msec = int_of_string ss.(7);
            method iden = ss.(8);
            method vers = ss.(9);
            method prio = prio_of_string ss.(10);
            method func = ss.(11);
            method data = data
        end
let rec parse_line fin =
    match Tools.input_line fin with
    | None -> None
    | Some line -> Some (parse_entry line)

let parse_channel fin =
    let rec aux lineno ls = 
        try
            match parse_line fin with
            | None -> ls
            | Some e -> aux (lineno+1) (e :: ls) 
        with 
        | Unsupported_node line ->
            eprintf "%d:  Unsupported node [%s]\n%!" lineno line;
            aux (lineno+1) ls
        | e ->
            aux (lineno+1) ls
    in
    aux 0 []

let parse_file fname = Tools.with_in_file parse_channel fname
}

