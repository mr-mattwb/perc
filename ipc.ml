open Unix
open Printf
open Stdlib

type key_t
type time_t = int

external ftok : string -> int -> key_t = "cIpc_ftok"

module Sem = 
    struct
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

        external semget : key_t -> int -> get_flag list -> perms -> t = "cIpc_semget"
        external semop1 : t -> int -> int -> op_flag list -> unit = "cIpc_semop1"
        external semop : t -> buf list -> int -> unit = "cIpc_semop"
        external stat : t -> int -> semid_ds = "cIpc_stat"
        external set : t -> int -> int * int * int -> unit = "cIpc_set"
    end
