{
open Unix
open Printf
open Stdlib

module Ser =
    struct
        module type ELT = 
            sig
                type elt
                val of_str : string -> elt
                val to_str : elt -> string
            end
        module Int =
            struct
                type elt = int
                let of_str = int_of_string
                let to_str = string_of_int
            end
        module Str =
            struct
                type elt = string
                let of_str s = s
                let to_str s = s
            end
        module Opt(S : ELT) =
            struct
                type elt = S.elt option
                let of_str s = 
                    match s with
                    | "" -> None
                    | v -> Some (S.of_str v)
                let to_str = function
                    | None -> ""
                    | Some s -> S.to_str s
            end
        module List(S : ELT) =
            struct
                type elt = S.elt list
                let splitter = "ALOG_LIST_SEP"
                let sep = try Unix.getenv splitter with _  -> ";"
                let rex = Pcre.regexp sep
                let of_str s = List.map S.of_str (Pcre.split ~rex s)
                let to_str ls = 
                    let aux acc s = acc^sep^s in
                    match List.map S.to_str ls with
                    | [] -> ""
                    | x :: [] -> x
                    | x :: xs -> List.fold_left aux x xs
            end
    end

module Env =
    struct

        module type ELT = 
            sig
                type elt
                val get : unit -> elt
                val set : elt -> unit
            end

        module type VAR =
            sig
                val name : string
                val switches : string list
                val descr : string
            end
        module type VAR_DEFAULT =
            sig
                include VAR
                type elt
                val default : elt
            end

        module Args = Map.Make(String)
        let args = ref Args.empty
        module Make(S : Ser.ELT)(V : VAR_DEFAULT with type elt = S.elt) : ELT with type elt = V.elt =
            struct
                type elt = V.elt
                let set_str s = Unix.putenv V.name s
                let set v = set_str (S.to_str v)
                let rec get () =
                    try S.of_str (Unix.getenv V.name)
                    with _ -> set V.default; get()
                let () = 
                    let descr = sprintf "[%s] %s" V.name V.descr in
                    let spec = Arg.String (fun s -> set_str s) in
                    let add sw = args := Args.add sw (sw, spec, descr) !args in
                    List.iter add V.switches
            end
        module MakeOpt(S : Ser.ELT)(V : VAR) : ELT with type elt = S.elt option =
            struct
                module SO = Ser.Opt(S)
                type elt = S.elt option
                let set so = Unix.putenv V.name (SO.to_str so)
                let get () = SO.of_str (Unix.getenv V.name)
            end
        module MakeList(S : Ser.ELT)(V : VAR) : ELT with type elt = S.elt list =
            struct
                module SL = Ser.List(S)
                type elt = S.elt list
                let set x = Unix.putenv V.name (SL.to_str x)
                let get () = try SL.of_str (Unix.getenv V.name) with Not_found -> []
                    
            end
        module type VAR_STR =
            sig
                val name : string
                val default : string
                val switches : string list
                val descr : string
            end
        module type VAR_INT = 
            sig
                val name : string
                val default : int
                val switches : string list
                val descr : string
            end
        module type VAR_STR_OPT =
            sig
                val name : string
                val default : string option
                val switches : string list
                val descr : string
            end
        module MakeStr(V : VAR_STR) = Make(Ser.Str)(
            struct
                type elt = string
                include V
            end)
        module MakeInt(V : VAR_INT) = Make(Ser.Int)(
            struct
                type elt = int
                include V
            end)
        module MakeStrOpt(V : VAR_STR_OPT) = Make(Ser.Opt(Ser.Str))(
            struct
                type elt = string option
                include V
            end)
    end
module NDF =
    struct
        type idate = int
        type itime = int
        type priority = DEBUG | INFO | WARN | ERROR

        type 'a t = {
            date : idate;
            time : itime;
            msec : int;
            iden : string;
            vers : string;
            prio : priority;
            func : string;
            data : 'a
        }

        let date_of_ints yr mo da = (yr * 10000) + (mo * 100) + da
        let date_of_strs yr mo da = date_of_ints (int_of_string yr) (int_of_string mo) (int_of_string da)
        let time_of_ints hr mi se = (hr * 10000) + (mi * 100) + se
        let time_of_strs hr mi se = time_of_ints (int_of_string hr) (int_of_string mi) (int_of_string se)
        let prio_of_str s =
            match String.uppercase_ascii s with
            | "DEBUG" -> DEBUG
            | "INFO" -> INFO
            | "WARN" -> WARN
            | "ERROR" -> ERROR
            | _ -> raise (Failure ("Invalid priority ["^s^"]"))

        type call_info = {
            dnis : string;
            ani : string;
            ucid : string;
            firstHistoryInfoUser : string;
            lastHistoryInfoUser : string;
            receivedUcid : string option;
            receivedUui : string option
        }
        type chain = {
            link_from : string;
            link_to : string
        }
        type call_type = 
            | Residential
            | Commercial
        type data = 
            | CallInfo of call_info
            | Chain of chain
            | CallType of call_type
            | Portal of string
            | Other of string

        let call_type_of_string s = 
            match String.uppercase_ascii s with
            | "RESIDENTIAL" -> Residential
            | "COMMERCIAL" -> Commercial
            | _ -> raise (Invalid_argument s)
    end
}
let wspc = [ ' ' '\t' ]
let startTkn = [^ '=' '#' ' ' '\t' ]
let endTkn = [^ '=' '#' ' ' '\t' ]
let tkn = [^ '=' '#' ]

let dig = ['0'-'9'] 
let hexdig = ['0' - '9' 'A'-'F' 'a'-'f']
let year = ['1' '2'] dig dig dig
let month = ('0' ['1'-'9'] | '1' ['0'-'2'])
let day = ('0' ['1'-'9'] | ['1'-'2'] dig | '3' ['0' '1'])
let hour = (['0'-'1'] dig | '2' ['0'-'3'])
let minute = ['0'-'5'] dig
let second = ['0'-'5'] dig
let msec = dig dig dig 
let identity = [^'|']*
let version = [^'-']*
let priority = ("DEBUG"|"INFO"|"WARN"|"ERROR")
let funcName = [^'|']*
let nulldig = dig* | "null"
let calltype = "Residential"|"Commercial"


rule pair = parse
    | wspc*                             { pair lexbuf }
    | '#' _*                            { pair lexbuf }
    | (startTkn tkn* endTkn*) as lhs    { String.trim lhs, second lexbuf }
and second = parse
    | wspc*                                         { second lexbuf }
    | '#' _*                                        { second lexbuf }
    | '=' wspc* (startTkn tkn* endTkn* as rhs)      { String.trim rhs }

and entry = parse
    | (year as yr) '-' (month as mo) '-' (day as da) 'T'
      (hour as hr) ':' (minute as mi) ':' (second as se) "," (msec as ms) "|" 
      (identity as iden) "|" (version as vers) "-" (priority as prio) [' ']* "|"
      (funcName as func) "|"  { 
        { NDF.date = NDF.date_of_strs yr mo da;
              time = NDF.time_of_strs hr mi se;
              msec = int_of_string ms;
              iden = iden;
              vers = vers;
              prio = NDF.prio_of_str prio;
              func = func;
              data = data lexbuf
        }
    }
and data = parse
    | "chaining from >" (_* as nfrom) "< to >" (_* as nto) "<" {
        NDF.Chain { link_from = nfrom; link_to = nto }
    }
    | "DNIS :" (dig* as dnis) " ANI :" (dig* as ani) " UCID :" (hexdig* as ucid) " FIRSTHISTORYINFOUSER :" (dig* as fhiu) " LASTHISTORYINFOUSER :" (dig* as lhiu) " RECEIVED_UCID :" (nulldig as rucid) " RECEIVED_UUI :" (nulldig as ruui) {
        NDF.CallInfo {
            dnis = dnis;
            ani = ani;
            ucid = ucid;
            firstHistoryInfoUser = fhiu;
            lastHistoryInfoUser = lhiu;
            receivedUcid = (match rucid with | "null" -> None | _ -> Some rucid);
            receivedUui = (match ruui with | "null" -> None | _ -> Some ruui)
        }
    }
    | "callType:["(calltype as ct)"]" {
        NDF.CallType (NDF.call_type_of_string ct)
    }
    | "portalName: ["(['A'-'z' '0'-'9' '_' '-']* as pn)"]" { NDF.Portal pn }
    | _* as data { NDF.Other data }

{
module Cfg =
    struct
        let parse_pair line = 
            let k, v = pair (Lexing.from_string line) in
            Unix.putenv k v
    
        let parse_channel fin = 
            match Tools.input_line fin with
            | None -> ()
            | Some line -> parse_pair line
            
        let parse_file fname =
            Tools.with_in_file parse_channel fname

        module ConfigFile = Env.MakeStr(
            struct
                let name = "CONFIG_FILE"
                let default = try Filename.chop_extension Tools.basename with _ -> Tools.basename
                let switches = [ "-f"; "--config-file" ]
                let descr = "Configuration file"
            end)
    end

module Log =
    struct
        type priority = Debug | Info | Warn | Error | Off
        type channel = Chan of out_channel | File of string | Disabled
        module PrioritySer = 
            struct
                type elt = priority
                let of_str s = 
                    match String.uppercase_ascii s with
                    | "DEBUG" -> Debug
                    | "INFO" -> Info
                    | "WARN" -> Warn
                    | "ERROR" -> Error
                    | "OFF" -> Off
                    | _ -> raise (Failure ("Invalid priority ["^s^"]"))
                let to_str = function
                    | Debug -> "DEBUG"
                    | Info -> "INFO"
                    | Warn -> "WARN"
                    | Error -> "ERROR"
                    | Off -> "OFF"
            end
        module ChannelSer = 
            struct
                type elt = channel
                let of_str s = 
                    match String.uppercase_ascii s with
                    | "STDERR" -> Chan stderr
                    | "STDOUT" -> Chan stdout
                    | "DISABLED" -> Disabled
                    | f -> File s
                let to_str = function
                    | Chan f when f = stderr -> "STDERR"
                    | Chan f when f = stdout -> "STDOUT"
                    | Chan _ -> raise (Failure "Cannot convert unknown channel to string")
                    | Disabled -> "DISABLED"
                    | File f -> f
            end
        module type VAR_PRIORITY = 
            sig
                val name : string
                val default : priority
                val switches : string list
                val descr : string
            end
        module type VAR_CHANNEL =
            sig
                val name : string
                val default : channel
                val switches : string list
                val descr : string
            end
        module MakePriority(V : VAR_PRIORITY) = Env.Make(PrioritySer)(
            struct
                type elt = priority
                include V
            end)
        module MakeChannel(V : VAR_CHANNEL) = Env.Make(ChannelSer)(
            struct
                type elt = channel
                include V
            end)

        module LogPriority = MakePriority(
            struct
                let name = "LOG_PRIORITY"
                let default = Info
                let switches = [ "--log-priority" ]
                let descr = "Set the default log priority"
            end)
        module LogChannel = MakeChannel(
            struct
                let name = "LOG_CHANNEL"
                let default = Chan stderr
                let switches = [ "--log-channel" ]
                let descr = "Set the default log channel"
            end)
        let message modn prio msg fout =
            let tm = Unix.localtime (Unix.time()) in
            fprintf fout "%04d-%02d-%02d %02d:%02d:%02d %s %8d %s %s\n%!"
               (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
               tm.tm_hour tm.tm_min tm.tm_sec
               modn (Unix.getpid()) (PrioritySer.to_str prio) msg
        module type ELT = 
            sig
                type elt
                val debug : elt -> ('a, unit, string, unit) format4 -> 'a
                val info : elt -> ('a, unit, string, unit) format4 -> 'a
                val warn : elt -> ('a, unit, string, unit) format4 -> 'a
                val error : elt -> ('a, unit, string, unit) format4 -> 'a
            end
        module Elt = 
            struct
                type elt = {
                    prio : priority;
                    chan : channel;
                    modn : string
                }
                let write modn prio msg = function
                    | File f -> Tools.with_append_file (message modn prio msg) f
                    | Chan fout -> message modn prio msg fout
                    | Disabled -> ()

                let debug log fmt =
                    let aux msg = write log.modn Debug msg log.chan in
                    if log.prio <= Debug then ksprintf aux fmt
                    else ksprintf (fun _ -> ()) fmt
                let info log fmt =
                    let aux msg = write log.modn Info msg log.chan in
                    if log.prio <= Info then ksprintf aux fmt
                    else ksprintf (fun _ -> ()) fmt
                let warn log fmt =
                    let aux msg = write log.modn Warn msg log.chan in
                    if log.prio <= Warn then ksprintf aux fmt
                    else ksprintf (fun _ -> ()) fmt
                let error log fmt =
                    let aux msg = write log.modn Error msg log.chan in
                    if log.prio <= Error then ksprintf aux fmt
                    else ksprintf (fun _ -> ()) fmt
            end
        include Elt
        let make p c m = {
            prio = p;
            chan = c;
            modn = m
        }
        let defaults ?(prio=LogPriority.get()) ?(chan=LogChannel.get()) modn = make prio chan modn
    end

module Entry =
    struct
        module LogFile = Log.MakeChannel(
            struct
                let name = "ENTRY_LOG_FILE"
                let default = Log.Disabled
                let switches = [ "--entry-log" ]
                let descr = "Entry log file name"
            end)
        let logger = Log.defaults ~chan:(LogFile.get()) "Entry"

        let parse line = 
            try
               Some (entry (Lexing.from_string line))
            with e ->
                Log.error logger "Error parsing input [%s] : [%s]" line (Printexc.to_string e);
                None

        let rec load_channel ls fin = 
            let aux = function
            | None -> ls
            | Some e -> e :: ls
            in
            match Tools.input_line fin with
            | None -> List.rev ls
            | Some line -> load_channel (aux (parse line)) fin

        let load_file fname = Tools.with_in_file (load_channel []) fname
    end
}
