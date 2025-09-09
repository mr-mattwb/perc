open Unix
open Printf
open Stdlib

open Tools

type cfg = 
    | Properties
    | Ini

module type PARAMS = 
    sig
        val name : string
        val desc : string
        val switches : string list
    end
module type DEFPARAMS = 
    sig
        type elt
        val default : elt
        include PARAMS
    end
module type DEFUNPARAMS =
    sig
        type elt
        val default : unit -> elt
        include PARAMS
    end
module type STR_PARAMS = 
    sig
        val default : string
        include PARAMS
    end 
module type INT_PARAMS = 
    sig
        val default : int
        include PARAMS
    end
module type INT32_PARAMS = 
    sig
        val default : int32
        include PARAMS
    end
module type INT64_PARAMS = 
    sig
        val default : int64
        include PARAMS
    end
module type FLT_PARAMS = 
    sig
        val default : float
        include PARAMS
    end
module type BOOL_PARAMS = 
    sig
        val default : bool
        include PARAMS
    end
module type FLAG_PARAMS = PARAMS
module type FILE_PARAMS = STR_PARAMS
module type PATH_PARAMS = 
    sig
        val path : unit -> file
    end
module type CMD_PARAMS = STR_PARAMS 
module type MULTI_PARAMS = PARAMS 
module type UCID_PARAMS = PARAMS

module type ELT =
    sig
        include DEFUNPARAMS
        val of_string : string -> elt
        val to_string : elt -> string
        val args : (Arg.key * Arg.spec * Arg.doc) list
        val get : unit -> elt
        val put : elt -> unit
    end
module type STR_ELT = ELT with type elt = string
module type INT_ELT = ELT with type elt = int
module type INT32_ELT = ELT with type elt = int32
module type INT64_ELT = ELT with type elt = int64
module type FLT_ELT = ELT with type elt = float
module type BOOL_ELT = ELT with type elt = bool
module type PATH = 
    sig
        val path : unit -> file
        val exists : unit -> bool
        val file : unit -> file option
        val base : unit -> file
        val dir : unit -> file
        val is_dir : unit -> bool
        val touch : unit -> unit
        val mkdir : ?perms:int -> unit -> unit
    end
module type FILE_ELT =
    sig
        include ELT with type elt = file
        include PATH
    end
module type SFILE_ELT =
    sig
        module Dir : FILE_ELT
        module File : FILE_ELT
        include PATH
    end
module type CMD_ELT = 
    sig
        include ELT with type elt = cmd
        val run : unit -> return_code
        val run_args : cmd -> return_code
        val with_in : (in_channel -> 'a) -> string -> 'a
    end

module type MULTI_ELT = 
    sig
        type t
        include ELT with type elt = t list
        val add : t -> unit
    end

module type UCID_ELT =
    sig
        include ELT with type elt = Tools.ucid 
        val create : unit -> elt
    end

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

module MakePath(E : PATH_PARAMS) : PATH
module File(P : FILE_PARAMS) : FILE_ELT
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

module Verbose : BOOL_ELT

val try_config_file : unit -> unit
val parse_config : file -> unit
val with_lex_file : (Lexing.lexbuf -> unit) -> file -> unit
val config : unit -> unit

