{
type priority = 
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
    | Off

let string_of_priority = function
    | Off -> "OFF"
    | Debug -> "DEBUG"
    | Info -> "INFO"
    | Warn -> "WARN"
    | Error -> "ERROR"
    | Fatal -> "FATAL"
let priority_of_string s =
    match String.uppercase_ascii s with
    | "OFF" -> Off
    | "DEBUG" -> Debug
    | "INFO" -> Info
    | "WARN" -> Warn
    | "ERROR" -> Error
    | "FATAL" -> Fatal
    | _ -> raise (Failure ("Priority Invalid ["^s^"]"))

let lprintf fout prio mn fmt =
    let writemsg msg =
        let tm = Unix.localtime (Unix.time()) in
        fprintf fout "%04d-%02d-%02d %02d:%02d:%02d %8d %8s %8s %s\n%!"
            (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday 
            tm.tm_hour tm.tm_min tm.tm_sec 
            (Unix.getpid()) (string_of_priority prio) mn msg
    and nothing msg = ()
    in
    match prio with
    | Off -> ksprintf nothing fmt
    | _ -> ksprintf writemsg fmt

module type LOG = 
    sig
        type elt
        val debug : elt -> ('a, unit, string, unit) format4 -> 'a
        val info : elt -> ('a, unit, string, unit) format4 -> 'a
        val warn : elt -> ('a, unit, string, unit) format4 -> 'a
        val error : elt -> ('a, unit, string, unit) format4 -> 'a
        val fatal : elt -> ('a, unit, string, unit) format4 -> 'a
    end

module Log =
    struct
        type elt = {
            prio : priority;
            name : string;
            chan : out_channel
        }
        let defaults ?(prio=Debug) ?(chan=stderr) n = {
            prio = prio;
            name = n;
            chan = chan
        }
        let off = defaults ~prio:Off ""
        let stdout prio = defaults ~prio ~chan:stdout 

        let write (parms : elt) p fmt = 
            if p >= parms.prio then
                lprintf parms.chan p parms.name fmt
            else
                ksprintf (fun _ -> ()) fmt

        let debug p fmt = write p Debug fmt
        let info p fmt = write p Info fmt
        let warn p fmt = write p Warn fmt
        let error p fmt = write p Error fmt
        let fatal p fmt = write p Fatal fmt

    end 

let date_of_ints yr mo da = (yr * 10000) + (mo * 100) + da
let time_of_ints hr mi se = (hr * 10000) + (mi * 100) + se

let date_of_strs yr mo da = date_of_ints (int_of_string yr) (int_of_string mo) (int_of_string da)
let time_of_strs hr mi se = time_of_ints (int_of_string hr) (int_of_string mi) (int_of_string se)

type dir = 
    | Start
    | End

type init_configuration = {
    path : string;
    dir : string
}

module BusinessUnitTable = Map.Make(String)
type business_unit_table = string BusinessUnitTable.t

type data = 
    | Other of string
    | InvocationCounter of dir
    | ExecutingSQL of string
    | ReadFromDBFinished of int
    | InitConfiguration of init_configuration
    | BusinessUnitTable of business_unit_table
type entry = {
    date : int;
    time : int;
    msec : int;
    iden : string;
    vers : string;
    prio : priority;
    func : string;
    data : data
}

let dir_of_string = function
    | "start" -> Start
    | "end" -> End
    | s -> raise (Failure ("Invalid dir ["^s^"]"))

}
rule entry = parse
    | (['1''2']['0'-'9']['0'-'9']['0'-'9'] as year) 
        '-' (('0'['1'-'9']|'1'['0'-'2']) as month)
        '-' (('0'['1'-'9']|['1'-'2']['0'-'9']|'3'['0'-'1']) as day)
        'T' ((['0'-'1']['0'-'9']|'2'['0'-'3']) as hour)
        ':' ((['0'-'5']['0'-'9']) as minute)
        ':' ((['0'-'5']['0'-'9']) as second)
        ',' ((['0'-'9']['0'-'9']['0'-'9']) as msec)
        '|' (['0'-'9' 'A'-'F']* as iden)
        '|' ([^'-']* as vers)
        '-' (("DEBUG"|"INFO"|"WARN"|"ERROR") as prio) [' ']*
        '|' ([^'|']* as func) 
        '|' (_* as rest) eof {
            { 
              date = date_of_strs year month day;
              time = time_of_strs hour minute second;
              msec = int_of_string msec;
              iden = iden;
              vers = vers;
              prio = priority_of_string prio;
              func = func;
              data = data (Lexing.from_string rest)
            }
        }
and data = parse
    | "InvocationCounter.valueUnbound: call"(("start"|"end") as dir)" was called [0]  times but it should have been called exactly once"  {
        InvocationCounter (dir_of_string dir)
    }
    | "executing >" (_* as sql) "<" { ExecutingSQL sql }
    | "readFromDB finished. It took "(['0'-'9']+ as msec)" milliseconds to complete." {
        ReadFromDBFinished (int_of_string msec)
    }
    | "initConfiguration: "(_* as path) ':' (_* as dir) {
        InitConfiguration { path = path; dir = dir }
    }
    | "businessUnitTable={" (_* as but) "}"  {
        BusinessUnitTable (businessUnitEntries (Lexing.from_string but))
    }
    | (_* as other)               { Other other }

and businessUnitEntries = parse
    | ", "                          { businessUnitEntries lexbuf }
    | eof                           { BusinessUnitTable.empty  }
    | ([^'='',']* as key)'='([^',']* as value) {
        BusinessUnitTable.add key value (businessUnitEntries lexbuf)
    }

{
type rsp = 
    | Entry of entry
    | Exc of exn

let parse_entry line = 
    try Entry (entry (Lexing.from_string line))
    with e -> Exc e
        

let tester = "2025-08-25T12:11:26,371||MOD25.09.0.004-WARN |calllog.InvocationCounter|InvocationCounter.valueUnbound: callstart was called [0]  times but it should have been called exactly once"
let tester_data = "InvocationCounter.valueUnbound: callstart was called [0]  times but it should have been called exactly once"
let executing_sql = "2025-08-25T11:58:20,199||MOD25.09.0.004-DEBUG|dataaccess.DatabaseWrapper|executing >SELECT LASTHISTORYINFOUSER,PORTAL_NAME,WELCOME_PROMPT FROM IVR_CALLFLOW.IVR_GENERIC_PORTAL_INFO WHERE PORTAL_NAME LIKE 'bulk_portal%'<"
let readfromdbfinished = "2025-08-25T11:58:19,804||MOD25.09.0.004-DEBUG|dataaccess.DatabaseWrapper|readFromDB finished. It took 3939 milliseconds to complete."
let initconfiguration = "2025-08-25T05:17:55,486||MOD25.09.0.004-DEBUG|ivr.ConfigurationAccessor|initConfiguration: outageConfigPath : /usr/local/shared/nuance-mod-v25-09-0_qa_ncw_app-1/nuance/external_config/nuancemoddockerconfig/outage_config/qa//"
let busunittable = "2025-08-25T05:17:55,499||MOD25.09.0.004-INFO |ivr.ConfigurationManager|businessUnitTable={00=NAT, 24=81091000, 50=NAT, 51=CSGEAST, 52=CSGOHIO, 53=CSGEAST, 10=NAT, 54=NYC, 11=CSGEAST, 55=TX, 12=CSGOHIO, 56=CSGBHN, 13=CSGEAST, 57=NCHTR, 14=NYC, 58=PAC, 15=TX, 16=CSGBHN, 17=NCHTR, 18=PAC, 61=83471000, DEFAULT=NAT, 64=81091000, 21=83471000}"

}
