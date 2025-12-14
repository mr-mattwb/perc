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
        let default () = "hello world"
        let switches = [ "-m"; "--msg" ]
        let desc = "Message to send"
    end)
module Service = MEnv.Set(
    struct
        let name = "Service"
        let switches = [ "-s"; "--service" ]
        let desc = "Start a service"
    end)

let rec service key = 
    let mid = msgget key [Create] 0o777 in
    serve mid
and serve mid =
    let mt, msg = (recv mid : int * string) in
    match String.uppercase_ascii msg with
    | "QUIT" -> exit 0
    | uppmsg -> send mid mt uppmsg

let rec spawn key = 
    let pid, ps = Tools.spawn service key in
    print_status pid ps;
    exit 0
and print_status pid ps = 
    match ps with
    | WEXITED p2 -> printf "WEXITED [%d][%d]\n%!" pid p2
    | WSIGNALED s -> printf "WSIGNALED [%d][%d]\n%!" pid s
    | WSTOPPED s -> printf "WSTOPPED [%d][%d]\n%!" pid s

let rec client key = 
    let mid = msgget key [Create] 0o777 in
    match Msg.get() with
    | "" -> stdin_client mid
    | ms -> msg_client mid ms
and msg_client mid msg = 
    send mid (MType.get()) msg;
    let rc, rsp = (recv ~mtype:(MType.get()) mid : int * string) in
    printf "Response [%s]\n%!" rsp
and stdin_client mid = 
    match Tools.input_line stdin with
    | None -> printf "Exiting\n%!"
    | Some line -> serve_line mid line
and serve_line mid msg =
    send mid (MType.get()) msg;
    let rc, rsp = (recv ~mtype:(MType.get()) mid : int * string) in
    printf "Response [%s]\n%!" rsp;
    stdin_client mid


let rec main () =
    MEnv.config();
    let key = ftok (KeyFile.get()) (ProjId.get()) in
    if (Service.get()) then spawn key; 
    client key 

let () =
    if not !Sys.interactive then
        main ()
                        
