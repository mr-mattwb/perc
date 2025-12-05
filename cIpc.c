#include <stdio.h>
#include <errno.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define Val_key(v)          (Val_int(v))
#define Key_val(v)          (Int_val(v))

int Semop_flag[] = {
    IPC_NOWAIT,
    SEM_UNDO
};
int Semget_flag[] = {
    IPC_EXCL,
    IPC_CREAT
};


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
        printf("semget error [%d]\n", errno);
        caml_failwith("semget");
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
        printf("semop error [%d]\n", errno);
        caml_failwith("semop");
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
        printf("semop error [%d]\n", errno);
        caml_failwith("semop");
    }
    CAMLreturn(Val_unit);
}

value cIpc_stat(value v_sem, value v_num) {
    CAMLparam2(v_sem, v_num);
    CAMLlocal2(v_rc, v_perms);
    int semid = Int_val(v_sem);
    int semnum = Int_val(v_num);
    int semop = IPC_STAT;
    struct semid_ds ds;
    int rc = semctl(semid, semnum, semop, &ds);
    if (rc == -1) {
        printf ("semctl [%d]\n", errno);
        caml_failwith("semctl(IPC_STAT)");
    }
    v_perms = caml_alloc_tuple(7);
    Store_field(v_perms, 0, Val_key(ds.sem_perm.__key));
    Store_field(v_perms, 1, Val_int(ds.sem_perm.uid));
    Store_field(v_perms, 2, Val_int(ds.sem_perm.gid));
    Store_field(v_perms, 3, Val_int(ds.sem_perm.cuid));
    Store_field(v_perms, 4, Val_int(ds.sem_perm.cgid));
    Store_field(v_perms, 5, Val_int(ds.sem_perm.mode));
    Store_field(v_perms, 6, Val_int(ds.sem_perm.__seq));
    v_rc = caml_alloc_tuple(4);
    Store_field(v_rc, 0, v_perms);
    Store_field(v_rc, 1, Val_int(ds.sem_otime));
    Store_field(v_rc, 2, Val_int(ds.sem_ctime));
    Store_field(v_rc, 3, Val_int(ds.sem_nsems));
    CAMLreturn(v_rc);
}

value cIpc_set(value v_semid, value v_semnum, value v_args) {
    CAMLparam3(v_semid, v_semnum, v_args);
    struct semid_ds ds;
    int semid = Int_val(v_semid);
    int semnum = Int_val(v_semnum);
    ds.sem_perm.uid = Int_val(Field(v_args, 0));
    ds.sem_perm.gid = Int_val(Field(v_args, 1));
    ds.sem_perm.mode = Int_val(Field(v_args, 2));
    int rc = semctl(semid, semnum, IPC_SET, &ds);
    if (rc == -1) {
        printf("semctl(IPC_SET) [%d]\n", errno);
        caml_failwith("semctl(IPC_SET)");
    }
    CAMLreturn(Val_unit);
}
