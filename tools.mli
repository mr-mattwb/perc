
type file = string
type dir = string
type cmd = string
type seconds = int
type return_code = int

val name : file
val basename : file

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

val fold_channel : in_channel -> (string -> 'a -> 'a) -> 'a -> 'a
val map_channel : in_channel -> (string -> 'a) -> 'a list
val iter_channel : in_channel -> (string -> unit) -> unit

val fold_file : file -> (string -> 'a -> 'a) -> 'a -> 'a
val map_file : file -> (string -> 'a) -> 'a list
val iter_file : file -> (string -> unit) -> unit

val get_file_as_list : file -> string list

val with_temp_file : string -> string -> (file -> 'a) -> 'a

val spawn : ('a -> 'b) -> 'a -> int * Unix.process_status

val tolerint_of_string : string -> int
