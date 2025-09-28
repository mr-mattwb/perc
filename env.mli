open Unix
open Printf
open Stdlib

open Tools
open EnvParam

type unixflag
val gSkipArgs : unixflag

type arg = Arg.key * Arg.spec * Arg.doc
module SwMap : Map.S with type key = string

val gProgramArgs : arg SwMap.t ref
val gHelp : bool ref
val args : (string -> unit) -> string -> unit
val parse_args : string -> unit
val arg_default : unit -> unit

module Make(S : Ser.ELT)(P : DEFUNPARAMS with type elt = S.elt) : ELT with type elt = P.elt

module List(S : Ser.ELT)(P : DEFUNPARAMS with type elt = S.elt list) : ELT with type elt = P.elt
module ListEmpty(S : Ser.ELT)(P : PARAMS) : ELT with type elt = S.elt list

module Str(P : STR_PARAMS) : STR_ELT
module StrEmpty(P : PARAMS) : STR_ELT
module Int(P : INT_PARAMS) : INT_ELT
module Int32(P : INT32_PARAMS) : INT32_ELT
module Int64(P : INT64_PARAMS) : INT64_ELT
module Int_0(P : PARAMS) : INT_ELT
module Flt(P : FLT_PARAMS) : FLT_ELT
module Flt_0(P : PARAMS) : FLT_ELT
module Bool(P : BOOL_PARAMS) : BOOL_ELT
module Set(P : FLAG_PARAMS) : BOOL_ELT
module Clear(P : FLAG_PARAMS) : BOOL_ELT

module MakeFile(P : FILE_PARAMS) : FILE_ELT
module DirFile(D : FILE_PARAMS)(F : FILE_PARAMS) : SFILE_ELT
module Cmd(P : CMD_PARAMS) : CMD_ELT

module Option(S : ELT) : ELT with type elt = S.elt option

module type NONE = 
    sig
        type o
        val none : o
    end
module MakeOption(S : ELT)(N : NONE with type o = S.elt) : ELT with type elt = S.elt option

module Hide(E : ELT) : ELT with type elt = E.elt

module MultiValue(S : Ser.ELT)(P : PARAMS) : MULTI_ELT with type t = S.elt 

module Ucid(P : UCID_PARAMS) : ELT with type elt = ucid

module Rex(P : REX_PARAMS) : REX_ELT

module Verbose : BOOL_ELT

val try_config_file : unit -> unit
val parse_config : file -> unit
val with_lex_file : (Lexing.lexbuf -> unit) -> file -> unit
val config : unit -> unit

