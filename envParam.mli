open Unix
open Printf
open Stdlib
module Rxp = Str

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
        val default : unit -> string
        include PARAMS
    end 
module type INT_PARAMS = 
    sig
        val default : unit -> int
        include PARAMS
    end
module type INT32_PARAMS = 
    sig
        val default : unit -> int32
        include PARAMS
    end
module type INT64_PARAMS = 
    sig
        val default : unit -> int64
        include PARAMS
    end
module type FLT_PARAMS = 
    sig
        val default : unit -> float
        include PARAMS
    end
module type BOOL_PARAMS = 
    sig
        val default : unit -> bool
        include PARAMS
    end
module type FLAG_PARAMS = PARAMS
module type FILE_PARAMS = STR_PARAMS
module type CMD_PARAMS = STR_PARAMS 
module type MULTI_PARAMS = PARAMS 
module type UCID_PARAMS = PARAMS
module type REX_PARAMS = STR_PARAMS

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
module type FILE_ELT =
    sig
        include ELT with type elt = file
        include FileOps.ELT
    end
module type SFILE_ELT =
    sig
        module Dir : FILE_ELT
        module File : FILE_ELT
        include FileOps.ELT
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

module type REX_ELT = 
    sig
        include STR_ELT
        val rex : unit -> Rxp.regexp
        val split : string -> string list
    end

