
type file = string
type seconds = int

val name : string
val basename : string

val input_line : in_channel -> string option

val use : ('a -> 'b) -> ('b -> unit) -> ('b -> 'c) -> 'a -> 'c 
val with_in_file : (in_channel -> 'a) -> file -> 'a
val with_out_file : (out_channel -> 'a) -> file -> 'a
val with_append_file : (out_channel -> 'a) -> file -> 'a
val with_in_process : (in_channel -> 'a) -> string -> 'a

val file_size : file -> int
val buffer_file : file -> Buffer.t
val get_file : file -> string
val put_file : file -> string -> unit
val append_file : file -> string -> unit

val getenv : string -> string option

