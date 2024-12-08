open Base

type board =
  { rows : string list
  ; w : int
  ; h : int
  }

type pos =
  { r : int
  ; c : int
  }

type antenna =
  { p : pos
  ; name : char
  }

module Pos = struct
  type t = pos

  let compare (a : pos) (b : pos) =
    match Int.compare a.r b.r with
    | 0 -> Int.compare a.c b.c
    | n -> n
  ;;

  let sexp_of_t p = Sexp.List [ Int.sexp_of_t p.r; Int.sexp_of_t p.c ]
end

module PosSet = struct
  include Pos
  include Comparator.Make (Pos)
end

let to_board lines =
  { rows = lines; w = String.length (List.hd_exn lines); h = List.length lines }
;;

let all_positions board =
  List.cartesian_product (List.range 0 board.h) (List.range 0 board.w)
  |> List.map ~f:(fun (r, c) -> { r; c })
;;

let get (b : board) pos =
  let row = List.nth_exn b.rows pos.r in
  String.get row pos.c
;;

let group_by_name antennas =
  List.fold
    ~init:(Map.empty (module Char))
    ~f:(fun map a -> Map.add_multi ~key:a.name ~data:a.p map)
    antennas
  |> Map.data
;;

let on_board b p = p.r >= 0 && p.r < b.h && p.c >= 0 && p.c < b.w

let calc_antinodes init limit (p1, p2) =
  let dr = p2.r - p1.r
  and dc = p2.c - p1.c in
  Sequence.unfold ~init ~f:(fun n ->
    let a = { r = p2.r + (dr * n); c = p2.c + (dc * n) }
    and b = { r = p1.r - (dr * n); c = p1.c - (dc * n) } in
    if n < limit then Some ([ a; b ], n + 1) else None)
  |> Sequence.to_list
  |> List.concat
;;

let get_antinodes positions ~calc =
  List.cartesian_product positions positions
  |> List.filter ~f:(fun (p1, p2) -> Pos.compare p1 p2 <> 0)
  |> List.map ~f:calc
  |> List.concat
;;

let get_antennas board positions =
  positions
  |> List.filter ~f:(fun pos -> not (Char.equal (get board pos) '.'))
  |> List.map ~f:(fun pos -> { p = pos; name = get board pos })
;;

let () =
  let board = Tools.read_lines () |> to_board in
  let towers = all_positions board |> get_antennas board |> group_by_name in
  let count antennas ~calc =
    antennas
    |> List.map ~f:(get_antinodes ~calc)
    |> List.concat
    |> List.filter ~f:(fun p -> on_board board p)
    |> List.fold ~init:(Set.empty (module PosSet)) ~f:(fun acc p -> Set.add acc p)
    |> Set.length
  in
  let m1 = count towers ~calc:(calc_antinodes 1 2) in
  let m2 = count towers ~calc:(calc_antinodes 0 100) in
  Stdlib.Printf.printf "Solution 1: %d\n" m1;
  Stdlib.Printf.printf "Solution 2: %d\n" m2
;;
