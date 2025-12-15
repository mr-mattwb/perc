open Unix
open Printf
open Stdlib

open Tools
open Ipc
open Msg

module KeyFile = MEnv.MakeFile(
    struct
        let name = "KeyFile"
        let default () = "dune"
        let switches = [ "-k"; "--key" ]
        let desc = "File to generate key"
    end)
module ProjId = MEnv.Int(
    struct
        let name = "ProjId"
        let default () = 1
        let switches = [ "-p"; "--id" ]
        let desc = "Project ID.  Used to generate key"
    end)
module MType = MEnv.Int(
    struct
        let name = "MType"
        let default () = 1
        let switches = [ "-t"; "--mtype" ]
        let desc = "Message type"
    end)
module Msg = MEnv.Str(
    struct
        let name = "Message"
        let default () = ""
        let switches = [ "-m"; "--msg" ]
        let desc = "Message to send"
    end)
module Service = MEnv.Set(
    struct
        let name = "Service"
        let switches = [ "-s"; "--service" ]
        let desc = "Start a service"
    end)

module LogFile = MEnv.MakeFile(
    struct
        let name = "LOG_NAME"
        let default () = Tools.basename ^ ".log"
        let switches = [ "-l"; "--log-name" ] 
        let desc = "Log file name"
    end)
module VLog = Log.Make(
    struct
        let mod_name = Tools.basename
        let level () = Log.Debug
        let targets () = [ Log.File (LogFile.get()) ]
    end)

let verbose fmt =
    let aux msg = if (MEnv.Verbose.get()) then VLog.debug "%s" msg in
    ksprintf aux fmt

let rec spawn_service key = 
    let pid, rsp = Tools.spawn spawned_service key in
    verbose "%s" (str_of_proc_status pid rsp)
and str_of_proc_status pid ps =
    match ps with
    | WEXITED s -> sprintf "WEXITED[%d][%d]" pid s
    | WSTOPPED s -> sprintf "WSTOPPED[%d][%d]" pid s
    | WSIGNALED s -> sprintf "WSIGNALED[%d][%d]" pid s
  
and spawned_service key = 
    let mid = msgget key [Create] 0o777 in
    try
        service mid;
        verbose "S Service exiting";
        exit 0
    with e ->
        verbose "S Service exiting with error [%s]" (Printexc.to_string e);
        exit (-1)
and service mid = 
    let mt, msg = (Ipc.Msg.recv ~mtype:0 mid : int * string) in
    verbose "S Received message type[%d] : [%s]" mt msg;
    serve mid mt msg;
    service mid
and serve mid mt msg = 
    try
        match String.uppercase_ascii msg with
        | "EXIT"
        | "QUIT" -> serve_exit mid mt
        | _      -> serve_rsp mid mt msg
    with e ->
        verbose "S Failed with error [%s]" (Printexc.to_string e);
        (try Ipc.Msg.send mid mt (Printexc.to_string e) with _ -> ())
and serve_exit mid mt = 
    verbose "S Exiting channel [%d]" mt;
    Ipc.Msg.send mid mt "EXIT";
    exit 0
and serve_rsp mid mt msg = 
    verbose "S Serving channel [%d] Message [%s]" mt msg;
    Ipc.Msg.send mid mt msg

let rec run_client key =
    let mid = msgget key [Create] 0o777 in
    match Msg.get() with
    | "" -> stdin_client mid
    | msg -> msg_client mid msg
and msg_client mid msg = 
    let mtype = MType.get () in
    verbose "C Msg client [%d] [%s]" mtype msg;
    Ipc.Msg.send mid mtype msg;
    let rc, rsp = (Ipc.Msg.recv ~mtype mid : int * string) in
    verbose "C Response type[%d] : [%s]" rc rsp;
    exit 0
and stdin_client mid = 
    let mtype = MType.get() in
    verbose "C stdin client [%d]" mtype;
    match Tools.input_line stdin with
    | None -> exit 0
    | Some msg -> submit_client mid mtype msg
and submit_client mid mtype msg = 
    verbose "C Handle msg type[%d] : [%s]" mtype msg;
    Ipc.Msg.send mid mtype msg;
    verbose "C Send msg type[%d] : [%s]" mtype msg;
    let rc, rsp = (Ipc.Msg.recv ~mtype mid : int * string) in
    verbose "C Received response [%d] : [%s]" rc rsp;
    stdin_client mid

let main () = 
    MEnv.config();
    verbose "Starting KeyFile[%s] ProjId[%d]" (KeyFile.get()) (ProjId.get());
    let key = ftok (KeyFile.get()) (ProjId.get()) in
    try
        if Service.get() then spawn_service key
        else run_client key;
        verbose "Exiting";
        exit 0
    with e ->
        verbose "Exiting with error [%s]" (Printexc.to_string e);
        printf "Error [%s]\n%!" (Printexc.to_string e)

let () = 
    if not !Sys.interactive then
        main ()
    else
        ()
