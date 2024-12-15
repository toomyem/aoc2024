open Base
open Tools

let rec can_push_v board (r, c) d =
  match board.(r).(c) with
  | '#' -> false
  | '.' -> true
  | 'O' | '@' -> can_push_v board (r + d, c) d
  | '[' -> can_push_v board (r + d, c) d && can_push_v board (r + d, c + 1) d
  | ']' -> can_push_v board (r + d, c - 1) d && can_push_v board (r + d, c) d
  | ch -> failwith ("Invalid object " ^ String.of_char ch)
;;

let rec can_push_h board (r, c) d =
  match board.(r).(c) with
  | '#' -> false
  | '.' -> true
  | 'O' | '@' | '[' | ']' -> can_push_h board (r, c + d) d
  | ch -> failwith ("Invalid object " ^ String.of_char ch)
;;

let rec move_h board ((r, c) as pos) d =
  match board.(r).(c) with
  | '#' | '.' -> pos
  | ('O' | '@' | '[' | ']') as ch ->
    move_h board (r, c + d) d |> ignore;
    board.(r).(c) <- '.';
    board.(r).(c + d) <- ch;
    r, c + d
  | ch -> failwith ("Invalid object " ^ String.of_char ch)
;;

let rec move_v board ((r, c) as pos) d =
  match board.(r).(c) with
  | '#' | '.' -> pos
  | ('O' | '@') as ch ->
    move_v board (r + d, c) d |> ignore;
    board.(r).(c) <- '.';
    board.(r + d).(c) <- ch;
    r + d, c
  | '[' ->
    move_v board (r + d, c) d |> ignore;
    move_v board (r + d, c + 1) d |> ignore;
    board.(r).(c) <- '.';
    board.(r).(c + 1) <- '.';
    board.(r + d).(c) <- '[';
    board.(r + d).(c + 1) <- ']';
    r + d, c
  | ']' ->
    move_v board (r + d, c - 1) d |> ignore;
    move_v board (r + d, c) d |> ignore;
    board.(r).(c - 1) <- '.';
    board.(r).(c) <- '.';
    board.(r + d).(c - 1) <- '[';
    board.(r + d).(c) <- ']';
    r + d, c
  | ch -> failwith ("Invalid object " ^ String.of_char ch)
;;

let move_up board pos = if can_push_v board pos (-1) then move_v board pos (-1) else pos
let move_down board pos = if can_push_v board pos 1 then move_v board pos 1 else pos
let move_left board pos = if can_push_h board pos (-1) then move_h board pos (-1) else pos
let move_right board pos = if can_push_h board pos 1 then move_h board pos 1 else pos

let to_move ch =
  match ch with
  | '^' -> move_up
  | 'v' -> move_down
  | '<' -> move_left
  | '>' -> move_right
  | _ -> failwith ("Invalid move " ^ String.of_char ch)
;;

let find_robot board =
  let w, h = board_dim board in
  let coords = List.cartesian_product (List.range 0 h) (List.range 0 w) in
  coords |> List.find_exn ~f:(fun p -> equal_char (get_at board p) '@')
;;

let make_moves moves board =
  let pos = find_robot board in
  List.fold
    ~init:pos
    ~f:(fun pos dir ->
      let move = to_move dir in
      move board pos)
    moves
  |> ignore;
  board
;;

let scale board =
  Array.map board ~f:(fun row ->
    Array.map row ~f:(fun ch ->
      match ch with
      | '#' -> [ '#'; '#' ]
      | '@' -> [ '@'; '.' ]
      | 'O' -> [ '['; ']' ]
      | '.' -> [ '.'; '.' ]
      | _ -> failwith ("Invalid object " ^ String.of_char ch))
    |> Array.to_list
    |> List.concat
    |> List.to_array)
;;

let calc_gps board =
  let w, h = board_dim board in
  let coords = List.cartesian_product (List.range 0 h) (List.range 0 w) in
  List.fold
    ~init:0
    ~f:(fun acc (r, c) ->
      acc
      +
      match board.(r).(c) with
      | 'O' | '[' -> (r * 100) + c
      | _ -> 0)
    coords
;;

let () =
  let state = Tools.read_lines () |> List.group ~break:(fun _ a -> String.is_empty a) in
  let make_board () = List.hd_exn state |> List.map ~f:String.to_array |> List.to_array in
  let moves = List.nth_exn state 1 |> List.map ~f:String.to_list |> List.concat in
  let n1 = make_board () |> make_moves moves |> calc_gps in
  let n2 = make_board () |> scale |> make_moves moves |> calc_gps in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
