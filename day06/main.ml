open Base

type direction =
  | Up
  | Down
  | Left
  | Right

type pos =
  { r : int
  ; c : int
  }

type guard =
  { p : pos
  ; d : direction
  }

module PosSet = struct
  module Pos = struct
    type t = pos

    let compare (a : pos) (b : pos) =
      match Int.compare a.r b.r with
      | 0 -> Int.compare a.c b.c
      | n -> n
    ;;

    let sexp_of_t p = Sexp.List [ Int.sexp_of_t p.r; Int.sexp_of_t p.c ]
  end

  include Pos
  include Comparator.Make (Pos)
end

let make_pos r c = { r; c }

let find_guard (board : string list) : guard =
  let r, row = List.findi_exn ~f:(fun _ row -> String.contains row '^') board in
  let c = String.index_exn row '^' in
  { p = make_pos r c; d = Up }
;;

let empty_set = Set.empty (module PosSet)

let on_board board (p : pos) =
  let h = List.length board
  and w = String.length (List.hd_exn board) in
  p.r >= 0 && p.r < h && p.c >= 0 && p.c < w
;;

let get board (p : pos) =
  if on_board board p
  then (
    let row = List.nth_exn board p.r in
    String.get row p.c)
  else '.'
;;

let make_move board g =
  let pos_up = make_pos (g.p.r - 1) g.p.c in
  let pos_down = make_pos (g.p.r + 1) g.p.c in
  let pos_left = make_pos g.p.r (g.p.c - 1) in
  let pos_right = make_pos g.p.r (g.p.c + 1) in
  match g.d with
  | Up when Char.equal (get board pos_up) '#' -> { g with d = Right }
  | Up -> { g with p = pos_up }
  | Right when Char.equal (get board pos_right) '#' -> { g with d = Down }
  | Right -> { g with p = pos_right }
  | Down when Char.equal (get board pos_down) '#' -> { g with d = Left }
  | Down -> { g with p = pos_down }
  | Left when Char.equal (get board pos_left) '#' -> { g with d = Up }
  | Left -> { g with p = pos_left }
;;

type result =
  | OffBoard
  | Loop

let calc_result board =
  let g = ref (find_guard board) in
  let v = ref empty_set in
  let c = ref 0 in
  let max = 10000 in
  while on_board board !g.p && !c < max do
    v := Set.add !v !g.p;
    g := make_move board !g;
    c := !c + 1
  done;
  if !c = max then Loop, !v else OffBoard, !v
;;

let set_obstacle board r c =
  List.mapi board ~f:(fun i row ->
    if i <> r
    then row
    else
      String.mapi row ~f:(fun i ch ->
        if i = c && not (Char.equal ch '^') then '#' else ch))
;;

let check_loop board n (p : pos) =
  let { r; c } = p in
  let board = set_obstacle board r c in
  let result, _ = calc_result board in
  match result with
  | OffBoard -> n
  | Loop -> n + 1
;;

let () =
  let board = Tools.read_lines () in
  let _, v = calc_result board in
  let n1 = v |> Set.length in
  let n2 = Set.fold ~init:0 ~f:(check_loop board) v in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
