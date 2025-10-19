open Unix
open Printf
open Stdlib

type opt = 
  | LOG_PID
  | LOG_CONS
  | LOG_ODELAY
  | LOG_NDELAY
  | LOG_NOWAIT
  | LOG_PERROR

type facility = 
  | LOG_AUTH
  | LOG_AUTHPRIV
  | LOG_CRON
  | LOG_DAEMON
  | LOG_FTP
  | LOG_KERN
  | LOG_LPR
  | LOG_MAIL
  | LOG_NEWS
  | LOG_SYSLOG
  | LOG_USER
  | LOG_UUCP
  | LOG_LOCAL0
  | LOG_LOCAL1
  | LOG_LOCAL2
  | LOG_LOCAL3
  | LOG_LOCAL4
  | LOG_LOCAL5
  | LOG_LOCAL6
  | LOG_LOCAL7

type priority =
  | LOG_EMERG
  | LOG_ALERT
  | LOG_CRIT
  | LOG_ERR	
  | LOG_WARNING
  | LOG_NOTICE
  | LOG_INFO
  | LOG_DEBUG

val prio_of_string : string -> priority
val string_of_prio : priority -> string


val openlog : string -> opt list -> facility -> unit
val closelog : unit -> unit
val syslog : priority -> ('a, unit, string, unit) format4  -> 'a

module SerPrio : Ser.ELT with type elt = priority
module type ENV_PARAM_PRIO =
    sig
        val name : string
        val switches : string list
        val desc : string
        val default : unit -> priority
    end
module EnvPrio(P : ENV_PARAM_PRIO) : EnvParam.ELT with type elt = priority

