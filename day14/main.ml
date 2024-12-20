open Base

type robot =
  { x : int
  ; y : int
  ; dx : int
  ; dy : int
  }

type quadrants =
  { lu : int
  ; ru : int
  ; ld : int
  ; rd : int
  }

let ww, hh = 101, 103
let empty = { lu = 0; ru = 0; ld = 0; rd = 0 }
let adjust v m = if v < 0 then v + m else v

let to_robot line =
  let arr = Tools.to_int_list line |> List.to_array in
  { x = arr.(0); y = arr.(1); dx = adjust arr.(2) ww; dy = adjust arr.(3) hh }
;;

let draw_robots robots =
  let board = Array.make_matrix ~dimy:ww ~dimx:hh '.' in
  List.iter ~f:(fun r -> board.(r.y).(r.x) <- 'X') robots;
  Array.fold ~init:[] board ~f:(fun acc row -> String.of_array row :: acc) |> List.rev
;;

let move { x; y; dx; dy } = { x = (x + dx) % ww; y = (y + dy) % hh; dx; dy }
let move_all robots = List.map ~f:move robots

let update_quadrants q r =
  if r.x < ww / 2 && r.y < hh / 2
  then { q with lu = q.lu + 1 }
  else if r.x > ww / 2 && r.y < hh / 2
  then { q with ru = q.ru + 1 }
  else if r.x < ww / 2 && r.y > hh / 2
  then { q with ld = q.ld + 1 }
  else if r.x > ww / 2 && r.y > hh / 2
  then { q with rd = q.rd + 1 }
  else q
;;

let found_tree picture =
  match
    List.find ~f:(fun row -> String.is_substring ~substring:"XXXXXXXX" row) picture
  with
  | Some _ -> true
  | None -> false
;;

let rec search_tree i robots =
  let r = move_all robots in
  let picture = draw_robots r in
  if found_tree picture then i else search_tree (i + 1) r
;;

let () =
  let robots = Tools.read_lines () |> List.map ~f:to_robot in
  let q =
    Fn.apply_n_times ~n:100 move_all robots |> List.fold ~init:empty ~f:update_quadrants
  in
  let n1 = q.lu * q.ru * q.ld * q.rd in
  let n2 = search_tree 1 robots in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
