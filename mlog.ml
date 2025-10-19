open Unix
open Printf
open Stdlib
module Rxp = Str
open EnvParam
open Syslog

module Ident = Env.Str(
    struct
        let name = "log.ident"
        let switches = [ "--log-ident"; "--ident"; "-i" ]
        let desc = "Log identity name"
        let default () = Tools.basename
    end)
module UsePid = Env.Set(
    struct
        let name = "log.opt.pid"
        let switches = [ "--opt-pid"; "--pid"; "-p" ]
        let desc = "Log pid in message"
    end)
module UseCons = Env.Set(
    struct
        let name = "log.opt.console"
        let switches = [ "--opt-console"; "--console"; "-c" ]
        let desc = "Log to console too"
    end)
module UsePerror = Env.Set(
    struct
        let name = "log.opt.perror"
        let switches = [ "--opt-perror"; "--perror" ]
        let desc = "Include the LOG_PERROR flag"
    end)
module UseODelay = Env.Set(
    struct
        let name = "log.opt.odelay"
        let switches = [ "--opt-odelay"; "--odelay" ]
        let desc = "Add the LOG_ODELAY flag"
    end)
module UseNDelay = Env.Set(
    struct
        let name = "log.opt.ndelay"
        let switches = [ "--opt-ndelay"; "--ndelay" ]
        let desc = "Use the LOG_NDELAY flag"
    end)
module UseNoWait = Env.Set(
    struct
        let name = "log.opt.nowait"
        let switches = [ "--opt-nowait"; "--nowait"; "--now" ]
        let desc = "Include the LOG_NOWAIT flag"
    end)
module Prio = Syslog.EnvPrio(
    struct
        let name = "log.priority"
        let switches = [ "--log-prio"; "--prio"; "-P" ]
        let desc = "Log priority value"
        let default () = Syslog.LOG_INFO
    end)
module Message = Env.StrEmpty(
    struct
        let name = "log.message"
        let switches = [ "--log-message"; "--log-msg"; "--msg"; "-m"; "--message" ]
        let desc = "Log this message instead of reading from stdin."
    end)

open Syslog
let flags = [
        UseCons.get, Syslog.LOG_CONS;
        UsePid.get, Syslog.LOG_PID;
        UsePerror.get, Syslog.LOG_PERROR;
        UseNoWait.get, Syslog.LOG_NOWAIT;
        UseODelay.get, Syslog.LOG_ODELAY;
        UseNDelay.get, Syslog.LOG_NDELAY
    ]
let addflag flags (get, flag) = 
    if get() then (flag :: flags)
    else flags

let rec main () = 
    Env.config();
    let flags = List.fold_left addflag [] flags in
    openlog (Ident.get()) flags Syslog.LOG_USER;
    match Message.get() with
    | "" -> loopEof ()
    | msg -> syslog (Prio.get()) "%s" msg
and loopEof () = 
    try
        runLoop()
    with End_of_file -> ()
and runLoop () = 
    let line = input_line stdin in 
    syslog (Prio.get()) "%s" line;
    runLoop()


let () = 
    if not !Sys.interactive then
        try
            main()
        with e ->
            eprintf "Error [%s]\n%!" (Printexc.to_string e)
;;

