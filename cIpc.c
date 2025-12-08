#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/msg.h>
#include <sys/shm.h>

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#define Val_key(v)          (Val_int(v))
#define Key_val(v)          (Int_val(v))

#define Val_mem(v)         (Val_long(v))
#define Mem_val(v)         ((char *)Long_val(v))

int Semop_flag[] = {
    IPC_NOWAIT,
    SEM_UNDO
};
int Semget_flag[] = {
    IPC_EXCL,
    IPC_CREAT
};
int Msgget_flag[] = {
    IPC_EXCL,
    IPC_CREAT
};
int Msg_flag[] = {
    IPC_NOWAIT,
    MSG_NOERROR
};
int Shm_flag[] = {
    IPC_CREAT,
    IPC_EXCL
};
int Shmat_flag[] = {
    SHM_RND,
    SHM_EXEC,
    SHM_RDONLY
};

int Unix_errno[] = {
    EACCES,
    EFAULT,
    EIDRM,
    EINVAL,
    ENOMEM,
    EOVERFLOW,
    EPERM,
    EEXIST,
    ENOENT,
    ENFILE,
    ENOSPC,
    E2BIG,
    EAGAIN,
    EFAULT,
    EFBIG,
    EIDRM,
    EINTR,
    ERANGE
};

value cIpc_ipc_perm(struct ipc_perm ds) {
    CAMLparam0();
    CAMLlocal1(v_perms);
    v_perms = caml_alloc_tuple(7);
    Store_field(v_perms, 0, Val_key(ds.__key));
    Store_field(v_perms, 1, Val_int(ds.uid));
    Store_field(v_perms, 2, Val_int(ds.gid));
    Store_field(v_perms, 3, Val_int(ds.cuid));
    Store_field(v_perms, 4, Val_int(ds.cgid));
    Store_field(v_perms, 5, Val_int(ds.mode));
    Store_field(v_perms, 6, Val_int(ds.__seq));
    CAMLreturn(v_perms);
}

#define unix_error2(fn)        (caml_unix_error(errno, fn, caml_copy_string("")))
#define unix_error_ctl(fn, id)  (caml_unix_error(errno, fn, caml_copy_string(id)))

value cIpc_ftok(value v_path, value v_id) {
    CAMLparam2(v_path, v_id);
    CAMLreturn(Val_key(ftok(String_val(v_path), Int_val(v_id))));
}

value cIpc_semget(value v_key, value v_nsems, value v_semflg, value v_semperm) {
    CAMLparam4(v_key, v_nsems, v_semflg, v_semperm);
    key_t key = Key_val(v_key);
    int nsems = Int_val(v_nsems);
    int semflg = caml_convert_flag_list(v_semflg, Semget_flag);
    semflg = semflg | Int_val(v_semperm);
    int rc = semget(key, nsems, semflg);
    if (rc == -1) {
        unix_error2("semget");
    }
    CAMLreturn(Val_int(rc));
}

value cIpc_semop1(value v_sem, value v_num, value v_op, value v_flg) {
    CAMLparam4(v_sem, v_num, v_op, v_flg);
    int semid = Int_val(v_sem);
    struct sembuf sops[1];
    sops[0].sem_num = Int_val(v_num);
    sops[0].sem_op = Int_val(v_op);
    sops[0].sem_flg = caml_convert_flag_list(v_flg, Semop_flag);
    int rc = semop(semid, (struct sembuf *)&sops, 1);
    if (rc == -1) {
        unix_error2("semop1");
    }
    CAMLreturn (Val_unit);
}

value cIpc_semop(value v_sem, value v_sembuf, value v_len) {
    CAMLparam3(v_sem, v_sembuf, v_len);
    CAMLlocal1(v_cur);
    int len = Int_val(v_len);
    struct sembuf sops[len];
    int i;

    i=0;
    while (v_sembuf != Val_emptylist) {
        v_cur = Field(v_sembuf, 0);
        sops[i].sem_num = Int_val(Field(v_cur, 0));
        sops[i].sem_op = Int_val(Field(v_cur, 1));
        sops[i].sem_flg = caml_convert_flag_list(Field(v_cur, 2), Semop_flag);
        i = i + 1;
        v_sembuf = Field(v_sembuf, 1);
    }
    int semid = Int_val(v_sem);
    int rc = semop(semid, (struct sembuf *)sops, len);
    if ( rc == -1 ) {
        unix_error2("semop");
    }
    CAMLreturn(Val_unit);
}

value cIpc_semctl_stat(value v_sem, value v_num) {
    CAMLparam2(v_sem, v_num);
    CAMLlocal2(v_rc, v_perms);
    int semid = Int_val(v_sem);
    int semnum = Int_val(v_num);
    int semop = IPC_STAT;
    struct semid_ds ds;
    int rc = semctl(semid, semnum, semop, &ds);
    if (rc == -1) {
        unix_error_ctl("semctl", "IPC_STAT");
    }
    v_rc = caml_alloc_tuple(4);
    Store_field(v_rc, 0, cIpc_ipc_perm(ds.sem_perm));
    Store_field(v_rc, 1, Val_int(ds.sem_otime));
    Store_field(v_rc, 2, Val_int(ds.sem_ctime));
    Store_field(v_rc, 3, Val_int(ds.sem_nsems));
    CAMLreturn(v_rc);
}

value cIpc_semctl_set(value v_semid, value v_semnum, value v_args) {
    CAMLparam3(v_semid, v_semnum, v_args);
    struct semid_ds ds;
    int semid = Int_val(v_semid);
    int semnum = Int_val(v_semnum);
    ds.sem_perm.uid = Int_val(Field(v_args, 0));
    ds.sem_perm.gid = Int_val(Field(v_args, 1));
    ds.sem_perm.mode = Int_val(Field(v_args, 2));
    int rc = semctl(semid, semnum, IPC_SET, &ds);
    if (rc == -1) {
        unix_error_ctl("semctl", "IPC_SET");
    }
    CAMLreturn(Val_unit);
}

value cIpc_semctl_rmid(value v_semid) {
    CAMLparam1(v_semid);
    int rc = semctl(Int_val(v_semid), 0, IPC_RMID, NULL);
    if (rc == -1) {
        unix_error_ctl("semctl", "IPC_RMID");
    }
    CAMLreturn(Val_unit);
}

value cIpc_semctl_getall(value v_semid) {
    CAMLparam1(v_semid);
    CAMLlocal2(v_rc, v_head);
    int semid = Int_val(v_semid);
    struct semid_ds ds;
    int rc;
    rc = semctl(semid, 0, IPC_STAT, &ds);
    if (rc == -1) {
        unix_error_ctl("semctl", "IPC_STAT");
    }
    int nsems = ds.sem_nsems;
    unsigned short array[nsems];
    rc = semctl(semid, 0, GETALL, array);
    if (rc == -1) {
        unix_error_ctl("semctl", "GETALL");
    }
    v_head = Val_emptylist;
    for (int i = 0; i < nsems; ++i) {
        v_rc = caml_alloc_tuple(2);
        Store_field(v_rc, 0, Val_int(array[i]));
        Store_field(v_rc, 1, v_head);
        v_head = v_rc;
    } 
    CAMLreturn(v_head);
}

value cIpc_semctl_getncnt(value v_sem, value v_semnum) {
    CAMLparam2(v_sem, v_semnum);
    int ds;
    int rc = semctl(Int_val(v_sem), Int_val(v_semnum), GETNCNT, &ds);
    if (rc == -1) {
        unix_error_ctl("semctl","GETNCNT");
    }
    CAMLreturn(Val_int(rc));
}

value cIpc_semctl_getpid(value v_sem, value v_semnum) {
    CAMLparam2(v_sem, v_semnum);
    int ds;
    int rc = semctl(Int_val(v_sem), Int_val(v_semnum), GETPID, &ds);
    if (rc == -1) {
        unix_error_ctl("semctl","GETPID");
    }
    CAMLreturn(Val_int(rc));
}

value cIpc_semctl_getval(value v_sem, value v_semnum) {
    CAMLparam2(v_sem, v_semnum);
    int arg_val = 0;
    int rc = semctl(Int_val(v_sem), Int_val(v_semnum), GETVAL, &arg_val);
    if (rc == -1) {
        unix_error_ctl("semctl","GETVAL");
    }
    CAMLreturn(Val_int(rc));
}

value cIpc_semctl_getzcnt(value v_sem, value v_semnum) {
    CAMLparam2(v_sem, v_semnum);
    int ds;
    int rc = semctl(Int_val(v_sem), Int_val(v_semnum), GETZCNT, &ds);
    if (rc == -1) {
        unix_error_ctl("semctl","GETZCNT");
    }
    CAMLreturn(Val_int(rc));
}

value cIpc_semctl_setval(value v_sem, value v_semnum, value v_ds) {
    CAMLparam3(v_sem, v_semnum, v_ds);
    int ds;
    ds = Int_val(v_ds);
    int rc = semctl(Int_val(v_sem), Int_val(v_semnum), SETVAL, ds);
    if (rc == -1) {
        unix_error_ctl("semctl","SETVAL");
    }
    CAMLreturn(Val_unit);
}
value cIpc_semctl_setall(value v_sem, value v_list) {
    CAMLparam2(v_sem, v_list);
    CAMLlocal1(v_cur);
    int length = 0;
    v_cur = v_list;
    while (v_cur != Val_emptylist) {
        v_cur = Field(v_cur, 1);
        length = length +1;
    }
    int i = 0;
    unsigned short array[length];
    v_cur = v_list;
    while (v_cur != Val_emptylist) {
        array[i] = Int_val(Field(v_cur, 0));
        v_cur = Field(v_cur, 1);
        i = i + 1;
    }
    int rc = semctl(Int_val(v_sem), 0, SETALL, array);
    if (rc == -1) {
        unix_error_ctl("semctl","SETALL");
    }
    CAMLreturn(Val_unit);
}

value cIpc_msgget(value v_key, value v_flgs, value v_perms) {
    CAMLparam3(v_key, v_flgs, v_perms);
    key_t key = Key_val(v_key);
    int msgflg = caml_convert_flag_list(v_flgs, Msgget_flag);
    msgflg = msgflg | Int_val(v_perms);
    int msg = msgget(key, msgflg);
    if (msg == -1) {
        caml_unix_error(errno, "msgget", caml_copy_string(""));
    }
    CAMLreturn(Val_int(msg));
}

struct mymsg {
    long mtype;
    char mtext[1];
};

value cIpc_msgsnd(value v_mid, value v_msg, value v_flgs) {
    CAMLparam3(v_mid, v_msg, v_flgs);
    CAMLlocal2(v_mtype, v_mtext);
    char *ptr;
    struct mymsg *msg;
    int mid = Int_val(v_mid);
    int flgs = caml_convert_flag_list(v_flgs, Msg_flag);
    v_mtype = Field(v_msg, 0);
    v_mtext = Field(v_msg, 1);
    int length = sizeof(long)+caml_string_length(v_mtext);
    ptr = (char *)malloc(length);
    msg = (struct mymsg *)ptr;
    msg->mtype = Int_val(v_mtype);
    strncpy(msg->mtext, String_val(v_mtext), caml_string_length(v_mtext));
    int rc = msgsnd(mid, msg, length, flgs);
    free(ptr);
    if (rc == -1) {
        caml_unix_error(errno, "msgsnd", v_msg);
    }
    CAMLreturn(Val_unit);
}

value cIpc_msgrcv(value v_mid, value v_mtype, value v_len, value v_flgs) {
    CAMLparam4(v_mid, v_mtype, v_len, v_flgs);
    CAMLlocal1(v_rc);
    int mid = Int_val(v_mid);
    long mtype = Int_val(v_mtype);
    int length = Int_val(v_len);
    int flgs = caml_convert_flag_list(v_flgs, Msg_flag);
    char *ptr = malloc(sizeof(long) + length);
    struct mymsg *msg = (struct mymsg *)ptr;
    msg->mtype = mtype;
    int rc = msgrcv(mid, msg, length, mtype, flgs);
    if (rc == -1) {
        free(ptr);
        caml_unix_error(errno, "msgrcv", v_len);
    }
    msg->mtext[length] = '\0';
    v_rc = caml_alloc_tuple(2);
    Store_field(v_rc, 0, Val_long(msg->mtype));
    Store_field(v_rc, 1, caml_copy_string(msg->mtext));
    free(ptr);
    CAMLreturn(v_rc);
}

value cIpc_msgctl_stat(value v_msqid) {
    CAMLparam1(v_msqid);
    CAMLlocal1(v_rc);
    struct msqid_ds ds;
    int rc = msgctl(Int_val(v_msqid), IPC_STAT, &ds);
    if (rc == -1) {
        unix_error_ctl("msgctl","IPC_STAT");;
    }
    v_rc = caml_alloc_tuple(9);
    Store_field(v_rc, 0, cIpc_ipc_perm(ds.msg_perm));
    Store_field(v_rc, 1, Val_int(ds.msg_stime));
    Store_field(v_rc, 2, Val_int(ds.msg_rtime));
    Store_field(v_rc, 3, Val_int(ds.msg_ctime));
    Store_field(v_rc, 4, Val_int(ds.msg_cbytes));
    Store_field(v_rc, 5, Val_int(ds.msg_qnum));
    Store_field(v_rc, 6, Val_int(ds.msg_qbytes));
    Store_field(v_rc, 7, Val_int(ds.msg_lspid));
    Store_field(v_rc, 8, Val_int(ds.msg_lrpid));
    CAMLreturn(v_rc);
}

value cIpc_msgctl_set(value v_msqid, value v_flds) {
    CAMLparam2(v_msqid, v_flds);
    struct msqid_ds ds;
    ds.msg_qbytes = Int_val(Field(v_flds, 0));
    ds.msg_perm.uid = Int_val(Field(v_flds, 1));
    ds.msg_perm.gid = Int_val(Field(v_flds, 2));
    ds.msg_perm.mode = Int_val(Field(v_flds, 3));
    int rc = msgctl(Int_val(v_msqid), IPC_SET, &ds);
    if (rc == -1) {
        unix_error_ctl("msgctl", "IPC_SET");
    }
    CAMLreturn(Val_unit);
}

value cIpc_msgctl_rmid(value v_msqid) {
    CAMLparam1(v_msqid);
    struct msqid_ds ds;
    int rc = msgctl(Int_val(v_msqid), IPC_RMID, &ds);
    if (rc == -1) {
        unix_error_ctl("msgctl", "IPC_RMID");
    }
    CAMLreturn(Val_unit);
}

value cIpc_shmget(value v_key, value v_size, value v_flgs, value v_perms) {
    CAMLparam4(v_key, v_size, v_flgs, v_perms);
    int shmflg = caml_convert_flag_list(v_flgs, Shm_flag);
    int rc = shmget(Key_val(v_key), Int_val(v_size), shmflg | Int_val(v_perms));
    if (rc == -1) {
        printf("shmget [%d]\n", errno);
        caml_failwith("shmget");
    }
    CAMLreturn(Val_int(rc));
}
value cIpc_shmat(value v_shmid, value v_flags) {
    CAMLparam2(v_shmid, v_flags);
    int flags = caml_convert_flag_list(v_flags, Shmat_flag);
    char *rc = shmat(Int_val(v_shmid), NULL, flags);
    if (rc == (void *)-1) {
        printf("shmat [%d]\n", errno);
        caml_failwith("shmat");
    }
    CAMLreturn(Val_mem(rc));
}
value cIpc_shmdt(value v_mem) {
    CAMLparam1(v_mem);
    char *mem = Mem_val(v_mem);
    int rc = shmdt(mem);
    if (rc == -1) {
        printf("shmdt [%d]\n", errno);
        caml_failwith("shmdt");
    }
    CAMLreturn(Val_unit);
}
value cIpc_shm_write(value v_mem, value v_msg) {
    CAMLparam2(v_mem, v_msg);
    char *mem = Mem_val(v_mem);
    strncpy(mem, String_val(v_msg), caml_string_length(v_msg));
    CAMLreturn(Val_unit);
}
value cIpc_shm_read(value v_mem) {
    CAMLparam1(v_mem);
    char *mem = Mem_val(v_mem);
    CAMLreturn(caml_copy_string(mem));
}

value cIpc_shmctl_stat(value v_shmid) {
    CAMLparam1(v_shmid);
    CAMLlocal1(v_rc);
    struct shmid_ds ds;
    int rc = shmctl(Int_val(v_shmid), IPC_STAT, &ds);
    if (rc == -1) {
        unix_error_ctl("shmctl", "IPC_STAT");
    }
    v_rc = caml_alloc_tuple(8);
    Store_field(v_rc, 0, cIpc_ipc_perm(ds.shm_perm));
    Store_field(v_rc, 1, Val_int(ds.shm_segsz));
    Store_field(v_rc, 2, Val_int(ds.shm_atime));
    Store_field(v_rc, 3, Val_int(ds.shm_dtime));
    Store_field(v_rc, 4, Val_int(ds.shm_ctime));
    Store_field(v_rc, 5, Val_int(ds.shm_cpid));
    Store_field(v_rc, 6, Val_int(ds.shm_lpid));
    Store_field(v_rc, 6, Val_int(ds.shm_nattch));
    CAMLreturn(v_rc);
}

value cIpc_shmctl_set(value v_shmid, value v_flds) {
    CAMLparam2(v_shmid, v_flds);
    struct shmid_ds ds;
    ds.shm_perm.uid = Int_val(Field(v_flds, 0));
    ds.shm_perm.gid = Int_val(Field(v_flds, 1));
    ds.shm_perm.mode = Int_val(Field(v_flds, 2));
    int rc = shmctl(Int_val(v_shmid), IPC_SET, &ds);
    if (rc == -1) {
        unix_error_ctl("shmctl", "IPC_SET");
    }
    CAMLreturn(Val_unit);
}

value cIpc_shmctl_rmid(value v_shmid) {
    CAMLparam1(v_shmid);
    int rc = shmctl(Int_val(v_shmid), IPC_RMID, NULL);
    if (rc == -1) {
        unix_error_ctl("shmctl", "IPC_RMID");
    }
    CAMLreturn(Val_unit);
}

