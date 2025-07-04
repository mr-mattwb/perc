open Unix 
open Printf 
open Stdlib

open Tools
open Env

type mod_name = string
type out = 
    | Channel of out_channel
    | File of file

type level =
    | Off
    | Debug
    | Info
    | Warn
    | Error
    | Fatal

module type CHAN_PARAMS = 
    sig
        val mod_name : mod_name
        val level : level
    end
module type PARAMS = 
    sig
        include CHAN_PARAMS
        val targets : out list
    end

module type ELT = 
    sig
        val debug : ('a, unit, string, unit) format4 -> 'a
        val info : ('a, unit, string, unit) format4 -> 'a
        val warn : ('a, unit, string, unit) format4 -> 'a
        val error : ('a, unit, string, unit) format4 -> 'a
        val throw : exn -> ('a, unit, string, unit) format4 -> 'a
    end

val use : ('a -> 'b) -> ('b -> unit) -> ('b -> 'c) -> 'a -> 'c
val app_output_file : file -> (out_channel -> 'a) -> 'a
val open_out_app : file -> out_channel 
val with_append : file -> (out_channel -> 'a) -> 'a

module type LEVEL_SER = Ser.ELT with type elt = level
module type LEVEL_ENV = 
    sig
        val name : string
        val default : level
        val switch : string
        val descr : string
    end 
module LevelSer : LEVEL_SER
module LevelEnv(LP : LEVEL_ENV) : Env.ELT with type elt = level

module OutSer : Ser.ELT with type elt = out
module type OUT_ENV =
    sig
        val name : string
        val default : out
        val switch : string
        val descr : string
    end
module OutEnv(S : OUT_ENV) : Env.ELT with type elt = out

val msg_string: mod_name -> level -> string -> string
val msg_output : out_channel -> mod_name -> level -> string -> unit
val msg_out : out_channel -> mod_name -> string -> unit
val msg_outb : out_channel -> string -> unit

val to_string : string -> level -> ('a, unit, string, string) format4 -> 'a
val fprintf : out_channel -> mod_name -> level -> ('a, unit, string, unit) format4 -> 'a
val oprintf : mod_name -> level -> ('a, unit, string, unit) format4 -> 'a
val eprintf : mod_name -> level -> ('a, unit, string, unit) format4 -> 'a
val file_printf : file -> mod_name -> level -> ('a, unit, string, unit) format4 -> 'a
val buffer_printf : Buffer.t -> mod_name -> level -> ('a, unit, string, unit) format4 -> 'a

module Make(P : PARAMS) : ELT
module MakeSub(P : PARAMS) : ELT
module Stdout(P : CHAN_PARAMS) : ELT
module Stderr(P : CHAN_PARAMS) : ELT

(* LOGMODNAME : log name or basename if not found *)
module LogModName : Env.ELT with type elt = string
module LogModSubName : Env.ELT with type elt = string
(* LOGLEVEL : log level or Debug if not specified *)
module LogLevel : Env.ELT with type elt = level
module LogTarget : Env.ELT with type elt = out
module Enviro : ELT



