val read_line : unit -> string
val read_lines : unit -> string list
val read_board : unit -> char array array
val board_dim : char array array -> int * int
val get_at : char array array -> int * int -> char
val set_at : char array array -> int * int -> char -> unit
val update_at : char array array -> int * int -> f:(char -> char) -> unit
val to_int_list : string -> int list
val permute : 'a list -> 'a list list
val sum_of_ints : int list -> int
val str_of_ints : int list -> string
