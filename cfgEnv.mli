open Unix
open Printf
open Stdlib

type file = string

module type ELT =
    sig
        type elt
        val name : string
        val descr : string
        val arg : Arg.key * Arg.spec * Arg.doc
        val get : unit -> elt
        val put : elt -> unit
    end
module type FILE_ELT = ELT with type elt = file


module type SERIAL = 
    sig
        type elt 
        val of_string : string -> elt
        val to_string : elt -> string
    end

module type STR_PARAMS = 
    sig
        val default : string
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

module Make(S : SERIAL)(P : PARAMS with type elt = S.elt) : ELT with type elt = P.elt

module StrSer : SERIAL with type elt = string
module IntSer : SERIAL with type elt = int
module FltSer : SERIAL with type elt = float
module BoolSer : SERIAL with type elt = bool

module MakeStr(P : PARAMS with type elt = string) : ELT with type elt = string
module MakeInt(P : PARAMS with type elt = int) : ELT with type elt = int
module MakeFlt(P : PARAMS with type elt = float) : ELT with type elt = float
module MakeBool(P : PARAMS with type elt = bool) : ELT with type elt = bool
module Set(P : BOOL_PARAMS) : ELT with type elt = bool
module Clear(P : BOOL_PARAMS) : ELT with type elt = bool


module MakeFile(P : FILE_PARAMS) : FILE_ELT
module LogFile : FILE_ELT
module CfgFile : FILE_ELT

val config : unit -> unit

