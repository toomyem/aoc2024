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

let read_board = Tools.read_lines () |> List.map ~f:String.to_array |> List.to_array

let () =
  let board = read_board in
  let areas =
    List.map ~f:(fill board) (coords board)
    |> List.filter ~f:(fun a -> not (List.is_empty a))
  in
  let n1 = List.map ~f:(fence1 board) areas |> Tools.sum_of_ints in
  Stdlib.Printf.printf "Solution 1: %d\n" n1
;;
