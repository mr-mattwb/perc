open Unix
open Printf
open Stdlib

type key_t
type time_t = int
type perms = int
type size_t = int
type shmatt_t = int
type pid_t = int
type msgnum_t = int
type msglen_t = int

type ipc_perm = {
    __key : key_t;
    uid : int;
    gid : int;
    cuid : int;
    cgid : int;
    mode : int;
    __seq : int;
}

type getflag = 
    | Create
    | Excl

val ftok : string -> int -> key_t

module Sem :
    sig
        type t
        type op_flag = 
            | NoWait
            | Undo

        type buf = {
            num : int;
            op : int;
            flg : op_flag list
        }   

        type semid_ds = {
            perm : ipc_perm;
            otime : time_t;
            ctime : time_t;
            nsems : int;
        } 

        type semset = {
            set_uid : int;
            set_gid : int;
            set_perms : perms
        }

        val semget : key_t -> int -> getflag list -> perms -> t 
        val semop1 : t -> int -> int -> op_flag list -> unit 
        val semop : t -> buf list -> unit

        val stat : t -> semid_ds 
        val set : t -> semset -> unit 
        val rmid : t -> unit
        val getall : t -> int list
        val getncnt : t -> int -> int
        val getpid : t -> int -> int
        val getval : t -> int -> int 
        val getzcnt : t -> int -> int
        val setval : t -> int -> int -> unit
        val setall : t -> int list -> unit

        val create : ?num:int -> ?flags:getflag list -> ?perms:perms -> key_t -> t
        
        val wait : ?num:int -> t -> unit
        val signal : ?num:int -> t -> unit
        val protect : ?num:int -> t -> ('a -> 'b) -> 'a -> 'b

    end

module Msg :
    sig
        type t
        type flag =
            | NoWait
            | NoError

        type msqid_ds = {
            msg_perm : ipc_perm;
            msg_stime : time_t;
            msg_rtime : time_t;
            msg_ctime : time_t;
            msg_cbytes : int;
            msg_qnum : msgnum_t;
            msg_qbytes : msglen_t;
            msg_lspid : pid_t;
            msg_lrpid : pid_t
        }

        type msgset = {
            set_qbytes : msglen_t;
            set_uid : int;
            set_gid : int;
            set_perms : perms
        }

        val msgget : key_t -> getflag list -> perms -> t
        val msgsnd : t -> int -> bytes -> int -> flag list -> unit
        val msgrcv : t -> int -> bytes -> int -> flag list -> int

        val stat : t -> msqid_ds 
        val set : t -> msgset -> unit
        val rmid : t -> unit 

        val create : ?flags:getflag list -> ?perms:perms -> key_t -> t

        val send : ?max:int -> ?buf:bytes -> t -> int -> 'a -> unit
        val recv : ?max:int -> ?buf:bytes -> ?mtype:int -> t -> int * 'a

        val send_str : t -> int ->  string -> unit
        val recv_str : ?max:int -> ?mtype:int -> t -> int * string
    end

module Shm :
    sig
        type t
        type shmatflag =
            | Rnd
            | Exec
            | Rdonly
        type mem_t

        type shmid_ds = {
            shm_perm : ipc_perm;
            shm_segsz : size_t;
            shm_atime : time_t;
            shm_dtime : time_t;
            shm_ctime : time_t;
            shm_cpid : pid_t;
            shm_lpid : pid_t;
            shm_nattch : shmatt_t
        }

        type shmset = {
            set_uid : int;
            set_gid : int;
            set_perms : perms;
        }

        val shmget : key_t -> int -> getflag list -> perms -> t 
        val shmat : t -> shmatflag list -> mem_t 
        val shmdt : mem_t -> unit

        val _write : mem_t -> bytes -> int -> unit
        val _read : mem_t -> bytes -> int -> unit
        val write_str : mem_t -> string -> unit
        val read_str : mem_t -> string

        val write : ?max:int -> ?buf:bytes -> mem_t -> 'a -> int
        val read : ?max:int -> ?buf:bytes -> mem_t -> 'a

        val stat : t -> shmid_ds 
        val set : t -> shmset -> unit 
        val rmid : t -> unit

        type hnd = t * mem_t
        val create : ?size:int -> ?flags:getflag list -> ?perms:perms -> key_t -> hnd
        val destroy : hnd -> unit
        val send : ?max:int -> ?buf:bytes -> hnd -> 'a -> int
        val recv : ?max:int -> ?buf:bytes -> hnd -> 'a 

        val use : ?create:(?size:size_t -> ?flags:getflag list -> ?perms:perms -> key_t -> hnd) -> key_t -> (mem_t -> 'a) -> 'a
    end

