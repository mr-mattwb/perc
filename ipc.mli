open Unix
open Printf
open Stdlib

type key_t
type time_t = int
type perms = int

val ftok : string -> int -> key_t

module Sem :
    sig
        type t
        type semflag = 
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

        val semget : key_t -> int -> semflag list -> perms -> t 
        val semop1 : t -> int -> int -> op_flag list -> unit 
        val semop : t -> buf list -> unit
        val stat : t -> int -> semid_ds 
        val set : t -> int -> int * int * int -> unit 
        val rmid : t -> unit
        val getall : t -> int list
        val getncnt : t -> int -> int
        val getpid : t -> int -> int
        val getval : t -> int -> int 
        val getzcnt : t -> int -> int
        val setval : t -> int -> int -> unit
        val setall : t -> int list -> unit
    end

module Msg :
    sig
        type t
        type msgflag = 
            | Excl
            | Create
        type flag =
            | NoWait
            | NoError

        type msgnum_t = int
        type msglen_t = int

        type ipc_perm = {
            __key : key_t;
            uid : int;
            gid : int;
            cuid : int;
            cgid : int;
            mode : int;
            __seq : int
        }
        type msqid_ds = {
            msg_perm : ipc_perm;
            msg_stime : time_t;
            msg_rtime : time_t;
            msg_ctime : time_t;
            msg_cbytes : int;
            msg_qnum : msgnum_t;
            msg_qbytes : msglen_t;
            msg_lspid : int;
            msg_lrpid : int
        }

        type ctlset = {
            set_qbytes : msglen_t;
            set_uid : int;
            set_gid : int;
            set_perms : perms
        }

        val msgget : key_t -> msgflag list -> perms -> t
        val msgsnd : t -> int * string -> flag list -> unit
        val msgrcv : t -> int -> int -> flag list -> int * string
        val stat : t -> msqid_ds 
        val set : t -> ctlset -> unit
        val rmid : t -> unit 
    end

module Shm :
    sig
        type t
        type shmflag = 
            | Create
            | Excl
        type shmatflag =
            | Rnd
            | Exec
            | Rdonly
        type mem_t

        val shmget : key_t -> int -> shmflag list -> perms -> t 
        val shmat : t -> shmatflag list -> mem_t 
        val shmdt : mem_t -> unit
        val write : mem_t -> string -> unit
        val read : mem_t -> string
    end

