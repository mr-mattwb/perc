open Unix
open Printf
open Stdlib
module Rxp = Str

open Tools

module type ELT =
    sig
        type elt
        val of_string : string -> elt
        val to_string : elt -> string
    end

module Str =
    struct
        type elt = string
        let of_string v = String.trim v
        let to_string v = String.trim v
    end
module Int = 
    struct
        type elt = int
        let of_string v = int_of_string (String.trim v)
        let to_string = string_of_int
    end
module Int32 =
    struct
        type elt = int32
        let of_string = Int32.of_string
        let to_string = Int32.to_string
    end
module Int64 =
    struct
        type elt = int64
        let of_string = Int64.of_string
        let to_string = Int64.to_string
    end
module Flt = 
    struct
        type elt = float
        let of_string v = float_of_string (String.trim v)
        let to_string = string_of_float
    end
module Bool = 
    struct
        type elt = bool
        let of_string str =
            try bool_of_string str
            with _ ->
                match str.[0] with
                | 'Y' | 'y' | 't' | 'T' | '1' -> true
                | _ -> false
        let to_string = string_of_bool
    end

module List(E : ELT) =
    struct
        open Lex
        type elt = E.elt list
        let of_string s = List.map (fun e -> E.of_string e) (item_list s)
        let to_string s = 
            let buf = Buffer.create 1024 in
            let rec aux = function 
               | [] -> 
                    Buffer.contents buf
               | y :: ys -> 
                    Buffer.add_char buf gSeparator; 
                    Buffer.add_string buf (E.to_string y);
                    aux ys
            in
            match s with
            | [] -> ""
            | x :: [] -> (E.to_string x)
            | x :: xs -> Buffer.add_string buf (E.to_string x); aux xs
    end

module Option(E : ELT) : ELT with type elt = E.elt option =
    struct
        type elt = E.elt option
        let of_string str = 
            match str with
            | "" -> None
            | ss -> Some (E.of_string ss)
        let to_string = function
            | None -> ""
            | Some ss -> E.to_string ss
    end

module type OPTION = 
    sig
        val none : string
    end
module MakeOption(E : ELT)(O : OPTION) = 
    struct
        type elt = E.elt option
        let of_string = function
            | s when s=O.none -> None
            | s -> Some (E.of_string s)
        let to_string = function
            | None -> O.none
            | Some s -> E.to_string s
    end

module Time = 
    struct
        type elt = int
        let sep = ":"
        let rsep = Rxp.regexp sep 

        let to_int = function
        | "" -> 0
        | ii -> int_of_string ii

        let of_string hms = 
            match Rxp.split rsep hms with
            | [] -> to_int hms
            | secs :: [] when String.contains hms ':' -> 3600 * (to_int secs)
            | secs :: [] -> to_int secs
            | hr :: min :: [] -> ((to_int hr)*3600) + ((to_int min)*60)
            | hr :: min :: secs :: _ -> ((to_int hr)*3600) + ((to_int min)*60) + (to_int secs)
        let to_string secs = 
            sprintf "%02d:%02d:%02d" (secs / 3600) ((secs /60) mod 60) (secs mod 60)

        let now () = 
            let tm = Unix.localtime (Unix.time()) in
            (tm.tm_hour * 3600) + (tm.tm_min * 60) + tm.tm_sec
        let to_int s = s
    end

module Ucid = 
    struct
        type elt = ucid
        let of_string = ucid_of_string
        let to_string = string_of_ucid
        let make () = new_ucid ()
    end
