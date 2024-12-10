open Base

module Pos = struct
  type t = int * int (* row, col *)

  let compare a b =
    match Int.compare (fst a) (fst b) with
    | 0 -> Int.compare (snd a) (snd b)
    | n -> n
  ;;
end

module Board = struct
  type t = string list

  type dir =
    | Up
    | Right
    | Down
    | Left

  let go_dir (r, c) dir =
    match dir with
    | Up -> r - 1, c
    | Right -> r, c + 1
    | Down -> r + 1, c
    | Left -> r, c - 1
  ;;

  let read_board : t = Tools.read_lines ()
  let dim board = List.length board, List.hd_exn board |> String.length

  let filter ~f board : Pos.t list =
    let h, w = dim board in
    List.cartesian_product (List.range 0 h) (List.range 0 w) |> List.filter ~f
  ;;

  let is_valid board (r, c) =
    let h, w = dim board in
    r >= 0 && r < h && c >= 0 && c < w
  ;;

  let get_at board pos =
    if is_valid board pos
    then (
      let row = List.nth_exn board (fst pos) in
      String.get row (snd pos))
    else '?'
  ;;

  let cell_is board pos ch = Char.equal (get_at board pos) ch
end

let rec trace board pos : Pos.t list list =
  let all_dirs = [ Board.Right; Down; Left; Up ] in
  if Board.is_valid board pos
  then
    if Board.cell_is board pos '9'
    then [ [ pos ] ]
    else (
      let lvl = Board.get_at board pos in
      let x =
        List.map all_dirs ~f:(fun d ->
          let np = Board.go_dir pos d in
          let nlvl = Char.of_int_exn (Char.to_int lvl + 1) in
          if Board.cell_is board np nlvl
          then List.map ~f:(fun lst -> pos :: lst) (trace board np)
          else [])
      in
      List.concat x)
  else []
;;

let dedup lst = List.dedup_and_sort lst ~compare:Pos.compare

let score (trails : Pos.t list list) =
  List.map ~f:(fun trail -> List.last_exn trail) trails |> dedup |> List.length
;;

let rating (trails : Pos.t list list) = List.length trails

let () =
  let board = Board.read_board in
  let heads = Board.filter ~f:(fun pos -> Board.cell_is board pos '0') board in
  let trails = List.map ~f:(fun pos -> trace board pos) heads in
  let n1 = List.map ~f:(fun trails -> score trails) trails |> Tools.sum_of_ints in
  let n2 = List.map ~f:(fun trails -> rating trails) trails |> Tools.sum_of_ints in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
