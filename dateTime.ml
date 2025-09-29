open Unix
open Printf   
open Stdlib
module Rxp = Str

type t = int * int

let empty = (0, 0)

let of_date_time date time = (date, time)
let make_date y m d = (y * 10000) + (m * 100) + d
let make_time h m s = (h * 3600) + (m * 60) + s
let make_date_time y m d h mn s = (make_date y m d, make_time h mn s)
let date_of_strs y m d = make_date (int_of_string y) (int_of_string m) (int_of_string d)
let time_of_strs h m s = make_time (int_of_string h) (int_of_string m) (int_of_string s)
let date_time_of_strs y m d h mn s = (date_of_strs y m d, time_of_strs h mn s)

module DateSep = Env.Rex(
    struct
        let name = "date.sep"
        let switches = [ "--date-sep"; "--ds" ]
        let desc = "Separate date fields"
        let default () = "-"
    end)
module TimeSep = Env.Rex(
    struct
        let name = "time.sep"
        let switches = [ "--time-sep"; "--ts" ]
        let desc = "Separate time fields"
        let default () = ":"
    end)
module DateTimeSep = Env.Rex(
    struct
        let name = "date.time.sep"
        let switches = [ "--date-time-sep"; "--dts" ]
        let desc = "Separate date from time"
        let default () = " "
    end)

let year (date, time) = date / 10000
let month (date, time) = (date / 100) mod 100
let day (date, time) = date mod 100
let hour (date, time) = time / 3600
let minute (date, time) = (time / 60) mod 60
let second (date, time) = time mod 60

let to_string dt = 
    sprintf "%04d%s%02d%s%02d%s%02d%s%02d%s%02d" 
        (year dt) (DateSep.get()) (month dt) (DateSep.get()) (day dt)
        (DateTimeSep.get())
        (hour dt) (TimeSep.get()) (minute dt) (TimeSep.get()) (second dt)

let rec of_string str = 
    let dtrxp = Rxp.regexp (DateTimeSep.get()) in
    match Rxp.split dtrxp str with
    | dt :: tm :: _ -> (date_of_str dt, time_of_str tm)
    | _ -> raise (Failure ("DateTime ["^str^"]"))
and date_of_str str = 
    let drxp = Rxp.regexp (DateSep.get()) in
    match Rxp.split drxp str with
    | yr :: mn :: dy :: _ -> date_of_strs yr mn dy
    | _ -> raise (Failure ("Date ["^str^"]"))
and time_of_str str =
    let trxp = Rxp.regexp (TimeSep.get()) in
    match Rxp.split trxp str with
    | hr :: mn :: sc :: _ -> time_of_strs hr mn sc
    | _ -> raise (Failure ("Time ["^str^"]"))

let now () = 
    let tm = Unix.localtime (Unix.time()) in
    (make_date_time (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday 
        tm.tm_hour tm.tm_min tm.tm_sec)

module Ser = 
    struct
        type elt = t
        let to_string = to_string
        let of_string = of_string
    end
    
module EnvMake(P : EnvParam.PARAMS) = Env.Make(Ser)(
    struct
        type elt = t
        include P
        let default () = now ()
    end)

