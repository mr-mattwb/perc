open Unix 
open Printf 
open Stdlib

open CfgEnv

type mod_name = string
type out = 
    | Out
    | Err
    | Channel of out_channel
    | File of file
    | Buffer of Buffer.t

module type ELT = 
    sig
        val printf : ('a, unit, string, unit) format4 -> 'b
    end

val use : ('a -> 'b) -> ('b -> unit) -> ('b -> 'c) -> 'a -> 'c
val app_output_file : file -> (out_channel -> 'a) -> 'a
val open_out_app : file -> out_channel 

val msg_string : mod_name -> string -> string
val msg_output : out_channel -> mod_name -> string -> unit

val to_string : string -> ('a, unit, string, string) format4 -> 'a
val fprintf : out_channel -> mod_name -> ('a, unit, string, unit) format4 -> 'a
val oprintf : mod_name -> ('a, unit, string, unit) format4 -> 'a
val eprintf : mod_name -> ('a, unit, string, unit) format4 -> 'a
val file_printf : file -> mod_name -> ('a, unit, string, unit) format4 -> 'a
val buffer_printf : Buffer.t -> mod_name -> ('a, unit, string, unit) format4 -> 'a
val write : out * mod_name -> ('a, unit, string, unit) format4 -> 'a

