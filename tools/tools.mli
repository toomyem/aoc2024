val read_line : unit -> string
val read_lines : unit -> string list
val read_board : unit -> char array array
val board_dim : char array array -> int * int
val get_at : char array array -> int * int -> char
val set_at : char array array -> int * int -> char -> unit
val update_at : char array array -> int * int -> f:(char -> char) -> unit
val is_at : char array array -> int * int -> char -> bool
val find_on_board : char array array -> char -> (int * int) option
val filter_board : char array array -> f:(int * int -> bool) -> (int * int) list
val to_int_list : string -> int list
val permute : 'a list -> 'a list list
val sum_of_ints : int list -> int
val str_of_ints : int list -> string
