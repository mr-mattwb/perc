open Unix
open Printf
open Stdlib

type out = 
    | Err of out_channel
    | File of CfgEnv.file
    | Buffer of Buffer.t

let msgpfx modn =
    let tm = Unix.localtime (Unix.time()) in
    sprintf "%04d-%02d-%02d %02d:%02d:%02d %8s %8d"
        (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday
        tm.tm_hour tm.tm_min tm.tm_sec 
        modn (getpid())


