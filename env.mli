open Unix
open Printf
open Stdlib

open Tools

module type ELT =
    sig
        type elt
        val name : string
        val descr : string
        val switch : string
        val arg : Arg.key * Arg.spec * Arg.doc
        val get : unit -> elt
        val put : elt -> unit
    end
module type FILE_ELT = ELT with type elt = file


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

type unixflag
val gSkipArgs : unixflag
val gSkipArgsIfInteractive : unixflag

type arg = Arg.key * Arg.spec * Arg.doc
module StrMap : Map.S with type key = string

val gProgramArgs : arg StrMap.t ref
val gHelp : bool ref
val args : (string -> unit) -> string -> unit
val parse_args : string -> unit
val arg_default : unit -> unit

module Make(S : Ser.ELT)(P : PARAMS with type elt = S.elt) : ELT with type elt = P.elt

module Str(P : STR_PARAMS) : ELT with type elt = string
module Int(P : INT_PARAMS) : ELT with type elt = int
module Flt(P : FLT_PARAMS) : ELT with type elt = float
module Bool(P : BOOL_PARAMS) : ELT with type elt = bool
module Set(P : BOOL_PARAMS) : ELT with type elt = bool
module Clear(P : BOOL_PARAMS) : ELT with type elt = bool

module File(P : FILE_PARAMS) : FILE_ELT
module CfgFile : FILE_ELT

val config : unit -> unit

