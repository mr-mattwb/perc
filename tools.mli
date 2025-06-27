
type file = string
type seconds = int

val name : string
val basename : string

val input_line : in_channel -> string option

val use : ('a -> 'b) -> ('b -> unit) -> ('b -> 'c) -> 'a -> 'c 
val with_in_file : (in_channel -> 'a) -> string -> 'a
val with_in_process : (in_channel -> 'a) -> string -> 'a
