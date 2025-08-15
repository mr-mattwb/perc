#include <stdio.h>
#include <string.h>
#include <hiredis/hiredis.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define Val_redis(v)            ((value)v)
#define Redis_val(v)            ((redisContext*)v)

value caml_redisConnect(value v_host, value v_port) {
    CAMLparam2(v_host, v_port);
    redisContext *redis = redisConnect(String_val(v_host), Int_val(v_port));
    CAMLreturn(Val_redis(redis));
}

