open Unix
open Printf
open Stdlib
module Rxp = Str

type t = int

let of_ints yr mn dy = (yr * 10000) + (mn * 100) + dy
let of_strs yr mn dy = of_ints (int_of_string yr) (int_of_string mn) (int_of_string dy)

let year0 = 0
let now () = 
    let tm = Unix.localtime (Unix.time()) in
    of_ints (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday

module Sep = Env.Rex(
    struct
        let name = "date.sep"
        let switches = [ "--date-sep"; "--ds" ]
        let desc = "Date field separator"
        let default () = "-"
    end)

let to_string ymd = 
    sprintf "%04d%s%02d%s%02d" (ymd / 10000) (Sep.get()) ((ymd / 100) mod 100) (Sep.get()) (ymd mod 100)
let of_string ymd = 
    match Rxp.split (Sep.rex()) ymd with
    | [] -> year0
    | days :: [] -> of_ints 0 0 (int_of_string days)
    | months :: days :: [] -> of_ints 0 (int_of_string months) (int_of_string days)
    | years :: months :: days :: _ -> of_ints (int_of_string years) (int_of_string months) (int_of_string days)

let to_int ymd = ymd
let of_int ymd = ymd

let date_pat = "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)"
let parse_date d = 
    let ss = Pcre.exec ~pat:date_pat d in
    let sar = Pcre.get_substrings ss in
    of_strs sar.(1) sar.(2) sar.(3)

module Ser = 
    struct
        type elt = t
        let to_string = to_string
        let of_string = of_string 
    end

module type ENV_PARAM = 
    sig
        val name : string val switches : string list
        val desc : string
    end
module type DEF_PARAM = 
    sig
        include ENV_PARAM
        val default : unit -> t
    end
module MakeEnv(P : DEF_PARAM) = Env.Make(Ser)(
    struct
        type elt = t
        include P
    end)
module Now(E : ENV_PARAM) = MakeEnv(
    struct
        include E
        let default () =
            let tm = Unix.localtime (Unix.time()) in
            of_ints (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
    end)
