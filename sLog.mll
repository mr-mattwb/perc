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

module type SER = 
    sig
        type elt
        val of_str : string -> elt
        val to_str : elt -> string
    end
module type ELT =
    sig
      type elt
      val name : string
      val default : elt
      val switch : string
      val desc : string
    end
module type ENV =
    sig
        type elt 
        val get : unit -> elt
        val set : elt -> unit
    end
module MakeEnv(Ser : SER)(Elt : ELT with type elt = Ser.elt) : ENV =
    struct
        type elt = Elt.elt
        let rec get () = 
            try Ser.of_str (Unix.getenv Elt.name)
            with _ -> set Elt.default; get()
        and set v =
            Unix.putenv Elt.name (Ser.to_str v)
    end
module MakeStr(E : ELT with type elt = string) = MakeEnv(struct type elt = string let to_str s = s let of_str s = s end)(E)

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

module PortalMap = Map.Make(Int)
type portal_map = string PortalMap.t

module CatCodeTable = Map.Make(String)
type cat_code_table = string CatCodeTable.t

type config_id = {
    configID : string;
    fileName : string
}
type null_value = {
    value : string;
    fileName : string
}

type data = 
    | Other of string
    | InvocationCounter of dir
    | ExecutingSQL of string
    | ReadFromDBFinished of int
    | InitConfiguration of init_configuration
    | BusinessUnitTable of business_unit_table
    | PortalMap of portal_map
    | CatCodeTable of cat_code_table
    | DBConfigFlagEmpty of config_id
    | NullValue of null_value
    | LoadConfiguration of string
    | Link of string * string

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

let dig = ['0'-'9']
let pmkey = dig dig

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
    | "initConfiguration: "(_* as path) ": " (_* as dir) {
        InitConfiguration { path = path; dir = dir }
    }
    | "businessUnitTable={" (_* as but) "}"  {
        BusinessUnitTable (businessUnitEntries (Lexing.from_string but))
    }
    | "PortalMap[{" (_* as pm) "}]" {
        PortalMap (portalMapEntries (Lexing.from_string pm))
    }
    | "categoryCodeTable={" (_* as cct) "}" {
        CatCodeTable (catCodeEntries (Lexing.from_string cct))
    }
    | "DB Config Flag Map is Empty for Config ID: " (_* as cfgID) " with fileName: " (_* as fname) {
        DBConfigFlagEmpty { configID = cfgID; fileName = fname }
    } 
    | "Null value for " (_* as nval) " in fileName: " (_* as fname)_ {
        NullValue { value = nval; fileName = fname }
    }
    | "loading configuration from local file repository: " (_* as fname) {
        LoadConfiguration fname
    }
    | "chaining from >" (_* as nfrom) "< to >" (_* as nto) "<" {
        Link (nfrom, nto)
}
    | (_* as other)               { Other other }
and catCodeEntries = parse
    | ',' [' ']*                    { catCodeEntries lexbuf }
    | eof                           { CatCodeTable.empty }
    | ([^'='',']* as key)'='([^',']* as value) {
        CatCodeTable.add key value (catCodeEntries lexbuf)
    }
and portalMapEntries = parse
    | ","[' ']*                     { portalMapEntries lexbuf }
    | eof                           { PortalMap.empty }
    | (pmkey as key) '=' ([^',']* as value) { 
        PortalMap.add (int_of_string key) value (portalMapEntries lexbuf) 
    }
and businessUnitEntries = parse
    | ", "                          { businessUnitEntries lexbuf }
    | eof                           { BusinessUnitTable.empty  }
    | ([^'='',']* as key)'='([^',']* as value) {
        BusinessUnitTable.add key value (businessUnitEntries lexbuf)
    }

and envPair = parse
    | [' ''\t']*                    { envPair lexbuf }
    | ([^ ' ' '\t' '=' '#']+ as key)[' ' '\t']* { equalpart key lexbuf }
    | '#' _*                        { envPair lexbuf }
and equalpart key = parse
    | [' ''\t']*                    { equalpart key lexbuf }
    | '='                           { valuepart key lexbuf }
    | '#' _*                        { key, "" }             
and valuepart key = parse
    | [' ' '\t']*                   { valuepart key lexbuf }
    | ([^ ' ' '\t'][^ '#']* as value)[' ' '\t']* { key, String.trim value }

{
type rsp = 
    | Entry of entry
    | Exc of exn

let parse_entry line = 
    try Entry (entry (Lexing.from_string line))
    with e -> Exc e

let err = Log.defaults Tools.basename

let rec parse_channel fin = 
    let rec aux ?(lineno=0) ls = 
        match Tools.input_line fin with
        | None -> ls
        | Some line ->
            aux ~lineno:(lineno+1) 
                (match parse_entry line with 
                | Entry e -> e :: ls
                | Exc e -> 
                    Log.error err "%d:  Failure [%s]" lineno (Printexc.to_string e);
                    ls)
    in aux []
let parse_file fname = Tools.with_in_file parse_channel fname

module CallSet = Set.Make(String)
module CallMap = Map.Make(String)

let all_links ls = List.fold_left (fun acc v -> match v.data with Link _ -> v :: acc | _ -> acc) [] ls
let call_ids ls = CallSet.elements (List.fold_left (fun acc v -> CallSet.add v.iden acc) CallSet.empty ls)

let call_links ls = 
    let folder map v = 
        match CallMap.find_opt v.iden map with
        | None -> CallMap.add v.iden (v.data :: []) map
        | Some ls -> CallMap.add v.iden (v.data :: ls) map
    in
    List.fold_left folder CallMap.empty ls

}
