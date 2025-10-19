open Unix
open Printf
open Stdlib
open Tools


module type PARAMS =
    sig
        val path : unit -> file
    end

module type ELT =
    sig
        include PARAMS
        val get_file : unit -> string
        val put_file : string -> unit
        val exists : unit -> bool
        val file : unit -> file option
        val base : unit -> file
        val dir : unit -> file
        val is_dir : unit -> bool
        val touch : unit -> unit
        val mkdir : ?perms:int -> unit -> unit
    end

module Make(P : PARAMS) =
    struct
        include P
        let get_file () = Tools.get_file (path())
        let put_file conts = Tools.put_file (path()) conts
        let exists () = Sys.file_exists (path())
        let file () =
            if exists() then Some (path())
            else None
        let base () = Filename.basename (path())
        let dir () = Filename.dirname (path())   
        let is_dir () = 
            match file() with
            | None -> false
            | Some f -> (stat f).st_kind = S_DIR
        let touch () = 
            let fname = path() in
            if Sys.file_exists fname && (stat fname).st_kind = S_DIR then
                Unix.closedir (Unix.opendir fname)
            else
                Tools.with_out_file flush fname
        let mkdir ?(perms=0o777) () = 
            try Unix.mkdir (path()) perms
            with Unix_error (EEXIST, "mkdir", _) -> ()
    end

