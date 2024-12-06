open Base

type direction =
  | Up
  | Down
  | Left
  | Right

type guard =
  { r : int
  ; c : int
  ; d : direction
  }

let find_guard (board : string list) : guard =
  let r, row = List.findi_exn ~f:(fun _ row -> String.contains row '^') board in
  let c = String.index_exn row '^' in
  { r; c; d = Up }
;;

let empty_set = Set.empty (module String)
let make_key g = Int.to_string g.r ^ "-" ^ Int.to_string g.c

let on_board board g =
  let h = List.length board
  and w = String.length (List.hd_exn board) in
  g.r >= 0 && g.r < h && g.c >= 0 && g.c < w
;;

let get board (r : int) (c : int) =
  if on_board board { r; c; d = Up }
  then (
    let row = List.nth_exn board r in
    String.get row c)
  else '.'
;;

let make_move board g =
  match g.d with
  | Up when Char.equal (get board (g.r - 1) g.c) '#' -> { g with d = Right }
  | Up -> { g with r = g.r - 1 }
  | Right when Char.equal (get board g.r (g.c + 1)) '#' -> { g with d = Down }
  | Right -> { g with c = g.c + 1 }
  | Down when Char.equal (get board (g.r + 1) g.c) '#' -> { g with d = Left }
  | Down -> { g with r = g.r + 1 }
  | Left when Char.equal (get board g.r (g.c - 1)) '#' -> { g with d = Up }
  | Left -> { g with c = g.c - 1 }
;;

type result =
  | OffBoard
  | Loop

let calc_result board =
  let g = ref (find_guard board) in
  let v = ref empty_set in
  let c = ref 0 in
  let max = 10000 in
  while on_board board !g && !c < max do
    v := Set.add !v (make_key !g);
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

let check_loop board n p =
  let r, c =
    match String.split ~on:'-' p |> List.map ~f:Int.of_string with
    | r :: c :: _ -> r, c
    | _ -> failwith "Error"
  in
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
