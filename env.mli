open Unix
open Printf
open Stdlib

open Tools

module type STR_PARAMS = 
    sig
        val default : string
        val name : string
        val descr : string
        val switch : string
    end 
module type INT_PARAMS = 
    sig
        val default : int
        val name : string
        val descr : string
        val switch : string
    end
module type FLT_PARAMS = 
    sig
        val default : float
        val name : string
        val descr : string
        val switch : string
    end
module type BOOL_PARAMS = 
    sig
        val default : bool
        val name : string
        val descr : string
        val switch : string
    end
module type PARAMS = 
    sig
        type elt
        val default : elt
        val name : string
        val descr : string
        val switch : string
    end 
module type FILE_PARAMS = STR_PARAMS
module type CMD_PARAMS = STR_PARAMS 

module type ELT =
    sig
        include PARAMS
        val of_string : string -> elt
        val to_string : elt -> string
        val arg : Arg.key * Arg.spec * Arg.doc
        val get : unit -> elt
        val put : elt -> unit
    end
module type STR_ELT = ELT with type elt = string
module type INT_ELT = ELT with type elt = int
module type FLT_ELT = ELT with type elt = float
module type BOOL_ELT = ELT with type elt = bool
module type FILE_ELT = 
    sig
        include ELT with type elt = file
        val exists : unit -> bool
        val file : unit -> file option
        val base : unit -> file
        val dir : unit -> dir
        val is_dir : unit -> bool
    end
module type CMD_ELT = 
    sig
        include ELT with type elt = cmd
        val run : unit -> return_code
        val run_args : cmd -> return_code
    end

type unixflag
val gSkipArgs : unixflag

type arg = Arg.key * Arg.spec * Arg.doc
module StrMap : Map.S with type key = string

val gProgramArgs : arg StrMap.t ref
val gHelp : bool ref
val args : (string -> unit) -> string -> unit
val parse_args : string -> unit
val arg_default : unit -> unit

module Make(S : Ser.ELT)(P : PARAMS with type elt = S.elt) : ELT with type elt = P.elt

module List(S : Ser.ELT)(P : PARAMS with type elt = S.elt list) : ELT with type elt = P.elt

module Str(P : STR_PARAMS) : STR_ELT
module Int(P : INT_PARAMS) : INT_ELT
module Flt(P : FLT_PARAMS) : FLT_ELT
module Bool(P : BOOL_PARAMS) : BOOL_ELT
module Set(P : BOOL_PARAMS) : BOOL_ELT
module Clear(P : BOOL_PARAMS) : BOOL_ELT

module File(P : FILE_PARAMS) : FILE_ELT
module Cmd(P : CMD_PARAMS) : CMD_ELT

module CfgFile : STR_ELT

module Option(S : ELT) : ELT with type elt = S.elt option

module type NONE = 
    sig
        type o
        val none : o
    end
module MakeOption(S : ELT)(N : NONE with type o = S.elt) : ELT with type elt = S.elt option

module Hide(E : ELT) : ELT with type elt = E.elt

val try_load_config_file : unit -> unit
val config : unit -> unit

