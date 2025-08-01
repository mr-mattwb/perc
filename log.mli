open Unix 
open Printf 
open Stdlib

open Tools
open Env

type mod_name = string
type level =
    | Off
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
type out = 
    | Channel of out_channel
    | File of Tools.file



module type PARAMS = 
    sig
        val mod_name : mod_name
        val level : unit -> level
        val targets : unit -> out list
    end

module type ELT = 
    sig
        val debug : ('a, unit, string, unit) format4 -> 'a
        val info : ('a, unit, string, unit) format4 -> 'a
        val warn : ('a, unit, string, unit) format4 -> 'a
        val error : ('a, unit, string, unit) format4 -> 'a
        val throw : exn -> ('a, unit, string, unit) format4 -> 'a
    end

module type LEVEL_SER = Ser.ELT with type elt = level
module type LEVEL_PARAMS = 
    sig
        val name : string
        val default : level
        val switches : string list
        val desc : string
    end 
module type LEVEL_ENV = Env.ELT with type elt = level

module type OUT_SER = Ser.ELT with type elt = out list
module type OUT_PARAMS =
    sig
        val name : string
        val default : out list
        val switches : string list
        val desc : string
    end
module type OUT_ENV = Env.ELT with type elt = out list

module LevelSer : LEVEL_SER
module LevelEnv(LP : LEVEL_ENV) : Env.ELT with type elt = level

module OutSerItem : Ser.ELT with type elt = out
module OutSer : Ser.ELT with type elt = out list
module OutEnv(P : OUT_PARAMS) : Env.ELT with type elt = out list

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
module Stdout(P : PARAMS) : ELT
module Stderr(P : PARAMS) : ELT

(* LOGMODNAME : log name or basename if not found *)
module ModName : Env.ELT with type elt = string
module ModSubName : Env.ELT with type elt = string

(* LOGLEVEL : log level or Debug if not specified *)
module Level : Env.ELT with type elt = level
module Targets : Env.ELT with type elt = out list
module Enviro : ELT

module type NAME = 
    sig
        val mod_name : mod_name
    end

(* Use the mod_name paraameter *)
(* Set the level and targets with Level and Targets modules *)
module Named(N : NAME) : ELT
module SubNamed(N : NAME)(SN : NAME) : ELT

