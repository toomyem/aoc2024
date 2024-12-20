open Base
open Tools

type board =
  { cells : int array array
  ; start_pos : int * int
  ; end_pos : int * int
  }

let all_dirs = [ 0; 1; 2; 3 ]

let compare a b =
  match Int.compare (fst a) (fst b) with
  | 0 -> Int.compare (snd a) (snd b)
  | n -> n
;;

let same_pos a b = compare a b = 0

let go dir (r, c) =
  match dir with
  | 0 -> r - 1, c
  | 1 -> r, c + 1
  | 2 -> r + 1, c
  | 3 -> r, c - 1
  | d -> failwith ("Invalid dir " ^ Int.to_string d)
;;

let move (r, c) (dr, dc) = r + dr, c + dc

let get_at cells (r, c) =
  let w, h = board_dim cells in
  if r >= 0 && r < h && c >= 0 && c < w then cells.(r).(c) else -1
;;

let set_at cells (r, c) v =
  let w, h = board_dim cells in
  if r >= 0 && r < h && c >= 0 && c < w then cells.(r).(c) <- v
;;

let find_on_board board ~f =
  let w, h = board_dim board in
  let coords = List.cartesian_product (List.range 0 h) (List.range 0 w) in
  List.find ~f:(fun (r, c) -> f board.(r).(c)) coords
;;

let calc_path { cells; start_pos; end_pos } =
  let rec walk pos i =
    set_at cells pos i;
    if same_pos pos end_pos
    then []
    else (
      let dir = List.find_exn all_dirs ~f:(fun dir -> get_at cells (go dir pos) = 0) in
      let npos = go dir pos in
      pos :: walk npos (i + 1))
  in
  walk start_pos 1
;;

let reach steps =
  List.cartesian_product
    (List.range ~stop:`inclusive (-steps) steps)
    (List.range ~stop:`inclusive (-steps) steps)
  |> List.filter ~f:(fun (r, c) ->
    let d = abs r + abs c in
    d > 1 && d <= steps)
;;

let count_cheats board path size =
  let cells = board.cells in
  let cheat_value (p1, p2) =
    get_at cells p2 - get_at cells p1 - abs (fst p1 - fst p2) - abs (snd p1 - snd p2)
  in
  let dest pos =
    List.map (reach size) ~f:(fun delta -> move pos delta)
    |> List.filter ~f:(fun pos -> get_at cells pos > 0)
  in
  let cheats =
    List.map path ~f:(fun pos -> dest pos |> List.map ~f:(fun pos2 -> pos, pos2))
    |> List.concat
  in
  List.count cheats ~f:(fun c -> cheat_value c >= 100)
;;

let () =
  let cells =
    read_lines ()
    |> List.map ~f:(fun line ->
      String.to_array line
      |> Array.map ~f:(fun ch ->
        match ch with
        | '.' -> 0
        | '#' -> -1
        | 'S' -> -2
        | 'E' -> -3
        | _ -> failwith "Invalid character"))
    |> List.to_array
  in
  let start_pos = find_on_board cells ~f:(fun v -> v = -2) |> Option.value_exn in
  let end_pos = find_on_board cells ~f:(fun v -> v = -3) |> Option.value_exn in
  set_at cells start_pos 0;
  set_at cells end_pos 0;
  let board = { cells; start_pos; end_pos } in
  let path = calc_path board in
  let n1 = count_cheats board path 2 in
  let n2 = count_cheats board path 20 in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
