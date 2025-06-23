open Unix
open Printf
open Stdlib

type mod_name = string
type out = 
    | Out
    | Err
    | Channel of out_channel
    | File of CfgEnv.file
    | Buffer of Buffer.t

type elt = out * mod_name

module type ELT = 
    sig
        val printf : ('a, unit, string, unit) format4 -> 'b
    end

let use ofn cfn ufn arg = 
    let hnd = ofn arg in
    try
        let rc = ufn hnd in
        cfn hnd;
        rc
    with e ->
        (try cfn hnd with _ -> ());
        raise e

let rec app_output_file fname fn = 
    use open_out_app close_out fn fname
and open_out_app fname = 
    open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 fname

let msg_string modn msg =
    let tm = Unix.localtime (Unix.time()) in
    sprintf "%04d-%02d-%02d %02d:%02d:%02d %8s %8d %s"
        (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday
        tm.tm_hour tm.tm_min tm.tm_sec 
        modn (getpid()) msg
let msg_output ch modn msg = 
    fprintf ch "%s\n%!" (msg_string modn msg)

let to_string modn fmt =
    let aux msg = msg_string modn msg in
    ksprintf aux fmt

let fprintf ch modn fmt =
    let aux msg = msg_output ch modn msg in
    ksprintf aux fmt

let oprintf modn fmt = 
    let aux msg = msg_output stdout modn msg in
    ksprintf aux fmt 

let eprintf modn fmt = 
    let aux msg = msg_output stderr modn msg in
    ksprintf aux fmt

let file_printf fname modn fmt = 
    let aux2 msg ch = msg_output ch modn msg in
    let aux msg = app_output_file fname (aux2 msg) in
    ksprintf aux fmt

let buffer_printf buf modn fmt = 
    let aux msg = 
        bprintf buf "%s\n%!" (msg_string modn msg)
    in
    ksprintf aux fmt

let write (target, modn) fmt = 
    let aux msg = 
        match target with
        | Channel ch -> msg_output ch modn msg
        | Out -> msg_output stdout modn msg
        | Err -> msg_output stderr modn msg
        | File fname -> app_output_file fname (fun ch -> msg_output ch modn msg)
        | Buffer buf -> bprintf buf "%s\n%!" (msg_string modn msg)
    in
    ksprintf aux fmt

let make out modn = (out, modn)


