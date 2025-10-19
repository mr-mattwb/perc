open Unix
open Printf
open Stdlib
module Rxp = Str

open PLog

module LogFile = Env.MakeFile(
    struct
        let name = "log.file"
        let switches = [ "--log-file"; "--lf" ]
        let desc = "The log file path"
        let default () = "ndf.log"
    end)
module Numbers = Env.Set(
    struct
        let name = "flag.numbers"
        let switches = [ "--numbers"; "--nums" ]
        let desc = "Display only the numeric nodes"
    end)


let rec per_call id nodes = 
    let m = DateTimeSet.min_elt nodes in
    printf "%s %s " id (DateTime.to_string m.time);
    DateTimeSet.iter per_node nodes;
    printf "\n%!"
and per_node entry = 
    if (Numbers.get()) then
        let node = fst entry.data in
        try
            let idx = String.index node '_' in
            let num = String.sub node 0 idx in
            printf "%s " num
        with e ->
            printf "%s " node
    else 
        printf "%s " (fst entry.data)
;;

let main () = 
    Env.config ();
    let module Logger = Log.Named(
        struct
            let mod_name = Tools.basename
        end)
    in
    Logger.info "Starting";
    try
        CallMap.iter per_call (PLog.load_file (LogFile.path()));
        Logger.info "Ending"
    with e ->
        Logger.error "Ending with error [%s]" (Printexc.to_string e);
        raise e

let () =
    if not !Sys.interactive then
    try main ()
    with e ->
        eprintf "%s:  Error [%s]\n%!" Tools.basename (Printexc.to_string e);
        exit (-1)


