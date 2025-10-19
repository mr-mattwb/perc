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

let prio_of_string s = 
    match String.uppercase_ascii s with
    | "EMERG" -> LOG_EMERG
    | "ALERT" -> LOG_ALERT
    | "CRIT" -> LOG_CRIT
    | "ERR" -> LOG_ERR
    | "WARNING" -> LOG_WARNING
    | "NOTICE" -> LOG_NOTICE
    | "INFO" -> LOG_INFO
    | "DEBUG" -> LOG_DEBUG
    | _ -> raise (Failure ("Invalid priority ["^s^"]"))
let string_of_prio = function
    | LOG_EMERG -> "EMERG"
    | LOG_ALERT -> "ALERT"
    | LOG_CRIT -> "CRIT"
    | LOG_ERR -> "ERR"
    | LOG_WARNING -> "WARNING"
    | LOG_NOTICE -> "NOTICE"
    | LOG_INFO -> "INFO"
    | LOG_DEBUG -> "DEBUG"

external openlog : string -> opt list -> facility -> unit = "csyslog_openlog"
external closelog : unit -> unit = "csyslog_closelog"
external csyslog : priority -> string -> unit = "csyslog_syslog"

let syslog prio fmt =
    ksprintf (csyslog prio) fmt

module SerPrio = 
    struct
        type elt = priority
        let to_string = string_of_prio
        let of_string = prio_of_string
    end
module type ENV_PARAM_PRIO = 
    sig
        val name : string
        val switches : string list
        val desc : string
        val default : unit -> priority
    end
    
module EnvPrio(P : ENV_PARAM_PRIO) = Env.Make(SerPrio)(
    struct
        type elt = priority
        include P
    end)

