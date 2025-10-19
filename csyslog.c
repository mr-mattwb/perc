#include <stdio.h>
#include <string.h>
#include <syslog.h>

#include <syslog.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

int optionsMap[] = {
    LOG_PID,
    LOG_CONS,
    LOG_ODELAY,
    LOG_NDELAY,
    LOG_NOWAIT,
    LOG_PERROR
};
#define Option_val(v)       (optionsMap[Int_val(v)])

int facilitiesMap[] = {
    LOG_AUTH,
    LOG_AUTHPRIV,
    LOG_CRON,
    LOG_DAEMON,
    LOG_FTP,
    LOG_KERN,
    LOG_LPR,
    LOG_MAIL,
    LOG_NEWS,
    LOG_SYSLOG,
    LOG_USER,
    LOG_UUCP,
    LOG_LOCAL0,
    LOG_LOCAL1,
    LOG_LOCAL2,
    LOG_LOCAL3,
    LOG_LOCAL4,
    LOG_LOCAL5,
    LOG_LOCAL6,
    LOG_LOCAL7
};
#define Facility_val(v)        (facilitiesMap[Int_val(v)])

value csyslog_openlog(value v_ident, value v_opt, value v_fac) {
    CAMLparam3(v_ident, v_opt, v_fac);
    openlog(String_val(v_ident), caml_convert_flag_list(v_opt, optionsMap), Facility_val(v_fac));
    CAMLreturn(Val_unit);
}
value csyslog_closelog() {
    CAMLparam0();
    closelog();
    CAMLreturn(Val_unit);
}
value csyslog_syslog(value v_prio, value v_msg) {
    CAMLparam2(v_prio, v_msg);
    syslog(Int_val(v_prio), "%s", String_val(v_msg));
    CAMLreturn(Val_unit);
}


