open Unix
open Printf
open Stdlib

open EnvParam

module Soxi : CMD_ELT
val with_soxi : string -> string -> string option
val with_soxi_int : string -> string -> int option
val with_soxi_float : string -> string -> float option
val file_type : string -> string option
val sample_rate : string -> int option
val channels : string -> int option
val samples : string -> int option
val duration_hms : string -> string option
val duration : string -> float option
val bits_per_sample : string -> int option
val avg_bitrate : string -> string option
val sample_precision : string -> int option
val audio_encoding : string -> string option
val comments : string -> string option

