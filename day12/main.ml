open Base

type direction =
  | Up
  | Down
  | Left
  | Right

let dirs = [ Up; Down; Left; Right ]

let move pos dir =
  let r, c = pos in
  match dir with
  | Up -> r - 1, c
  | Down -> r + 1, c
  | Left -> r, c - 1
  | Right -> r, c + 1
;;

let board_dim b = Array.length b, Array.length b.(0)

let coords b =
  let h, w = board_dim b in
  List.cartesian_product (List.range 0 h) (List.range 0 w)
;;

let get_at board pos =
  let r, c = pos in
  let h, w = board_dim board in
  if r >= 0 && r < h && c >= 0 && c < w then board.(r).(c) else '.'
;;

let set_at board pos ch =
  let r, c = pos in
  let h, w = board_dim board in
  if r >= 0 && r < h && c >= 0 && c < w then board.(r).(c) <- ch
;;

let rec fill board pos =
  let ch = get_at board pos in
  if not (Char.between ~low:'A' ~high:'Z' ch)
  then []
  else (
    set_at board pos (Char.lowercase ch);
    let w =
      List.map dirs ~f:(fun dir ->
        let npos = move pos dir in
        let nch = get_at board npos in
        if Char.equal ch nch then fill board npos else [])
    in
    pos :: List.concat w)
;;

let fence1 board area =
  let costs =
    List.map
      ~f:(fun pos ->
        let ch = get_at board pos in
        let f =
          List.map dirs ~f:(fun dir ->
            let npos = move pos dir in
            let nch = get_at board npos in
            if Char.equal ch nch then 1 else 0)
        in
        4 - Tools.sum_of_ints f)
      area
  in
  List.length area * Tools.sum_of_ints costs
;;

let in_area area pos =
  match List.find area ~f:(fun p -> fst pos = fst p && snd pos = snd p) with
  | Some _ -> true
  | None -> false
;;

let inc r = r := !r + 1

let fence2 board area =
  let h, w = board_dim board in
  let s = ref 0 in
  List.iter (List.range 0 h) ~f:(fun r ->
    let count_up = ref true in
    let count_down = ref true in
    List.iter (List.range 0 w) ~f:(fun c ->
      let pos = r, c in
      if in_area area pos
      then (
        let ch = get_at board pos in
        let ch_up = get_at board (move pos Up) in
        let ch_down = get_at board (move pos Down) in
        let ch_prev = get_at board (move pos Left) in
        if not (equal_char ch ch_prev)
        then (
          count_up := true;
          count_down := true);
        if equal_char ch ch_up
        then count_up := true
        else if !count_up
        then (
          inc s;
          count_up := false);
        if equal_char ch ch_down
        then count_down := true
        else if !count_down
        then (
          inc s;
          count_down := false))));
  List.iter (List.range 0 w) ~f:(fun c ->
    let count_left = ref true in
    let count_right = ref true in
    List.iter (List.range 0 h) ~f:(fun r ->
      let pos = r, c in
      if in_area area pos
      then (
        let ch = get_at board pos in
        let ch_left = get_at board (move pos Left) in
        let ch_right = get_at board (move pos Right) in
        let ch_prev = get_at board (move pos Up) in
        if not (equal_char ch ch_prev)
        then (
          count_left := true;
          count_right := true);
        if equal_char ch ch_left
        then count_left := true
        else if !count_left
        then (
          inc s;
          count_left := false);
        if equal_char ch ch_right
        then count_right := true
        else if !count_right
        then (
          inc s;
          count_right := false))));
  !s * List.length area
;;

let read_board = Tools.read_lines () |> List.map ~f:String.to_array |> List.to_array

let () =
  let board = read_board in
  let areas =
    List.map ~f:(fill board) (coords board)
    |> List.filter ~f:(fun a -> not (List.is_empty a))
  in
  let n1 = List.map ~f:(fence1 board) areas |> Tools.sum_of_ints in
  let n2 = List.map ~f:(fence2 board) areas |> Tools.sum_of_ints in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
