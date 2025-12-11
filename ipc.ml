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


external ftok : string -> int -> key_t = "cIpc_ftok"

module Sem = 
    struct
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

        external semget : key_t -> int -> getflag list -> perms -> t = "cIpc_semget"
        external semop1 : t -> int -> int -> op_flag list -> unit = "cIpc_semop1"
        external _semop : t -> buf list -> int -> unit = "cIpc_semop"

        let semop s str = _semop s str (List.length str)

        external stat : t -> semid_ds = "cIpc_semctl_stat"
        external set : t -> semset -> unit = "cIpc_semctl_set"
        external rmid : t -> unit = "cIpc_semctl_rmid"
        external _getall : t -> int list = "cIpc_semctl_getall"
        let getall s = List.rev (_getall s)
        external getncnt : t -> int -> int = "cIpc_semctl_getncnt"
        external getpid : t -> int -> int = "cIpc_semctl_getpid"
        external getval : t -> int -> int = "cIpc_semctl_getval"
        external getzcnt : t -> int -> int = "cIpc_semctl_getzcnt"
        external setval : t -> int -> int -> unit = "cIpc_semctl_setval"
        external setall : t -> int list -> unit = "cIpc_semctl_setall"

        let create ?(num=1) ?(flags=[Create]) ?(perms=0o777) key = semget key num flags perms
        let use ?(create=create) key fn =
            let sem = create key in
            try
                let rc = fn sem in
                rmid sem;
                rc
            with e ->
                (try rmid sem with _ -> ());
                raise e

        let wait ?(num=0) sem = semop sem [{num=0; op=(-1); flg=[]}] 
        let signal ?(num=0) sem = semop sem [{num=0; op=1; flg=[]}]
        let protect ?(num=0) sem fn arg = 
            wait ~num sem;
            try
                let rc = fn arg in
                signal ~num sem;
                rc
            with e ->
                (try signal ~num sem with _ -> ());
                raise e
    end 

module Msg = 
    struct
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

        external msgget : key_t -> getflag list -> perms -> t = "cIpc_msgget"
        external msgsnd : t -> int -> bytes -> int -> flag list -> unit = "cIpc_msgsnd"
        external msgrcv : t -> int -> bytes -> int -> flag list -> int = "cIpc_msgrcv"

        external stat : t -> msqid_ds = "cIpc_msgctl_stat"
        external set : t -> msgset -> unit = "cIpc_msgctl_set"
        external rmid : t -> unit = "cIpc_msgctl_rmid"

        let create ?(flags=[Create]) ?(perms=0o777)  key = msgget key flags perms
        let use ?(create=create) key fn = 
            let msg = create key in
            try
                let rc = fn msg in
                rmid msg;
                rc
            with e ->
                (try rmid msg with _ -> ());
                raise e

        let send ?(max=1024) ?(buf=Bytes.create max) mid mtype msg =
            let len = Marshal.to_buffer buf 0 (Bytes.length buf) msg [] in
            msgsnd mid mtype buf len []
        let recv ?(max=1024) ?(buf=Bytes.create max) ?(mtype=0) mid = 
            let mt = msgrcv mid mtype buf (Bytes.length buf) [] in
            mt, Marshal.from_bytes buf 0
        let send_str mid mtype str = 
            let buf = Marshal.to_bytes str [] in
            msgsnd mid mtype buf (Bytes.length buf) []
        let recv_str ?(max=1024) ?(mtype=0) mid = 
            let buf = Bytes.create max in
            let mt = msgrcv mid mtype buf max [] in
            mt, (Marshal.from_bytes buf 0 : string)
    end

module Shm =
    struct
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
            set_perms : perms
        }

        external shmget : key_t -> int -> getflag list -> perms -> t = "cIpc_shmget"
        external shmat : t -> shmatflag list -> mem_t = "cIpc_shmat"
        external shmdt : mem_t -> unit = "cIpc_shmdt"
        
        external _write : mem_t -> bytes -> int -> unit = "cIpc_shm_write"
        external _read : mem_t -> bytes -> int -> unit = "cIpc_shm_read"
        external write_str : mem_t -> string -> unit = "cIpc_shm_write_str"
        external read_str : mem_t -> string = "cIpc_shm_read_str"

        let write ?(max=1024) ?(buf=Bytes.create max) mem obj =
            let len = Marshal.to_buffer buf 0 max obj [] in
            _write mem buf len;
            len
        let read ?(max=1024) ?(buf=Bytes.create max) mem =
            _read mem buf (Bytes.length buf);
            Marshal.from_bytes buf 0

        external stat : t -> shmid_ds = "cIpc_shmctl_stat"
        external set : t -> shmset -> unit = "cIpc_shmctl_set"
        external rmid : t -> unit = "cIpc_shmctl_rmid"

        type hnd = t * mem_t

        let create ?(size=1024) ?(flags=[Create]) ?(perms=0o777) key = 
            let shm = shmget key size flags perms in
            let mem = shmat shm [] in
            (shm, mem)

        let destroy (shm, mem) = 
            shmdt mem;
            rmid shm

        let send ?(max=1024) ?(buf=Bytes.create max) (shm, mem) obj = write ~max ~buf mem obj
        let recv ?(max=1024) ?(buf=Bytes.create max) (shm, mem) = read ~max ~buf mem

        let use ?(create=create) key fn = 
            let (shm, mem) = create key in
            try
                let rc = fn mem in
                shmdt mem;
                rmid shm;
                rc
            with e ->
                (try shmdt mem; rmid shm with _ -> ());
                raise e
    end



