open Unix
open Printf
open Stdlib
module Rxp = Str

type t = int

let midnight = 0

let of_ints hh mm ss = (3600 * hh) + (60 * mm) + ss
let of_strs hh mm ss = of_ints (int_of_string hh) (int_of_string mm) (int_of_string ss)

module Sep = Env.Str(
    struct
        let name = "time.sep"
        let switches = [ "--time-sep"; "--ts" ]
        let desc = "Time field separator"
        let default () = ":"
    end)

let parse hhmmss =  
    match Rxp.split (Rxp.regexp (Sep.get())) hhmmss with
    | [] -> 0
    | ss :: [] -> of_ints 0 0 (int_of_string ss)
    | hh :: mm :: [] -> of_ints (int_of_string hh) (int_of_string mm) 0
    | hh :: mm :: ss :: _ -> of_ints (int_of_string hh) (int_of_string mm) (int_of_string ss)

let to_string nw = 
    sprintf "%02d%s%02d%s%02d" (nw / 3600) (Sep.get()) ((nw / 60) mod 60) (Sep.get()) (nw mod 60)
let of_string = parse

let now () = 
    let tm = Unix.localtime (Unix.time()) in
    of_ints tm.tm_hour tm.tm_min tm.tm_sec

let time_pat = "(\\d\\d):(\\d\\d):(\\d\\d)"
let parse_time line = 
    let ss = Pcre.exec ~pat:time_pat line in
    let tss = Pcre.get_substrings ss in
    of_strs tss.(1) tss.(2) tss.(3)

module Ser = 
    struct
        type elt = t
        let of_string = of_string
        let to_string = to_string
    end

module type ENV = EnvParam.ELT with type elt = t 

module type ENV_PARAM = EnvParam.PARAMS
module type DEF_PARAM = EnvParam.DEFUNPARAMS with type elt = t

module MakeEnv(P : DEF_PARAM) = Env.Make(Ser)(P)
module Midnight(P : ENV_PARAM) = MakeEnv(
    struct
        type elt = t
        include P
        let default () = midnight
    end)
module Now(P : ENV_PARAM) = MakeEnv(
    struct
        type elt = t
        include P
        let default () = now ()
    end)
    


