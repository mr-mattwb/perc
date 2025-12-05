open Unix
open Printf
open Stdlib

type key_t
type time_t = int

val ftok : string -> int -> key_t

module Sem :
    sig
        type t
        type perms = int
        type get_flag = 
            | Excl
            | Create
        type op_flag = 
            | NoWait
            | Undo

        type buf = {
            num : int;
            op : int;
            flg : op_flag list
        }   

        type ipc_perm = {
            __key : key_t;
            uid : int;
            gid : int;
            cuid : int;
            cgid : int;
            mode : int;
            __seq : int;
        }
        type semid_ds = {
            perm : ipc_perm;
            otime : time_t;
            ctime : time_t;
            nsems : int;
        } 

        val semget : key_t -> int -> get_flag list -> perms -> t 
        val semop1 : t -> int -> int -> op_flag list -> unit 
        val semop : t -> buf list -> int -> unit
        val stat : t -> int -> semid_ds 
        val set : t -> int -> int * int * int -> unit 
    end

