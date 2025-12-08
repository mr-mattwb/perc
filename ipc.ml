open Unix
open Printf
open Stdlib

type key_t
type time_t = int
type perms = int
type size_t = int
type shmatt_t = int
type pid_t = int
type ipc_perm = {
    __key : key_t;
    uid : int;
    gid : int;
    cuid : int;
    cgid : int;
    mode : int;
    __seq : int;
}

external ftok : string -> int -> key_t = "cIpc_ftok"

module Sem = 
    struct
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

        type semid_ds = {
            perm : ipc_perm;
            otime : time_t;
            ctime : time_t;
            nsems : int;
        } 

        external semget : key_t -> int -> semflag list -> perms -> t = "cIpc_semget"
        external semop1 : t -> int -> int -> op_flag list -> unit = "cIpc_semop1"
        external _semop : t -> buf list -> int -> unit = "cIpc_semop"
        let semop s str = _semop s str (List.length str)
        external stat : t -> int -> semid_ds = "cIpc_semctl_stat"
        external set : t -> int -> int * int * int -> unit = "cIpc_semctl_set"
        external rmid : t -> unit = "cIpc_semctl_rmid"
        external _getall : t -> int list = "cIpc_semctl_getall"
        let getall s = List.rev (_getall s)
        external getncnt : t -> int -> int = "cIpc_semctl_getncnt"
        external getpid : t -> int -> int = "cIpc_semctl_getpid"
        external getval : t -> int -> int = "cIpc_semctl_getval"
        external getzcnt : t -> int -> int = "cIpc_semctl_getzcnt"
        external setval : t -> int -> int -> unit = "cIpc_semctl_setval"
        external setall : t -> int list -> unit = "cIpc_semctl_setall"
    end 

module Msg = 
    struct
        type t
        type msgflag = 
            | Excl
            | Create
        type flag =
            | NoWait
            | NoError

        type msgnum_t = int
        type msglen_t = int

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

        type ctlset = {
            set_qbytes : msglen_t;
            set_uid : int;
            set_gid : int;
            set_perms : perms
        }

        external msgget : key_t -> msgflag list -> perms -> t = "cIpc_msgget"
        external msgsnd : t -> int * string -> flag list -> unit = "cIpc_msgsnd"
        external msgrcv : t -> int -> int -> flag list -> int * string = "cIpc_msgrcv"
        external stat : t -> msqid_ds = "cIpc_msgctl_stat"
        external set : t -> ctlset -> unit = "cIpc_msgctl_set"
        external rmid : t -> unit = "cIpc_msgctl_rmid"
    end

module Shm =
    struct
        type t
        type shmflag = 
            | Create
            | Excl
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
            set_perms : perms
        }

        external shmget : key_t -> int -> shmflag list -> perms -> t = "cIpc_shmget"
        external shmat : t -> shmatflag list -> mem_t = "cIpc_shmat"
        external shmdt : mem_t -> unit = "cIpc_shmdt"
        external write : mem_t -> string -> unit = "cIpc_shm_write"
        external read : mem_t -> string = "cIpc_shm_read"
        external stat : t -> shmid_ds = "cIpc_shmctl_stat"
        external set : t -> shmset -> unit = "cIpc_shmctl_set"
        external rmid : t -> unit = "cIpc_shmctl_rmid"
    end


