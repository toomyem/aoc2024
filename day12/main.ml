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

let fill board pos =
  let rec fill0 board pos =
    let ch = get_at board pos in
    if Char.equal ch '.'
    then []
    else (
      set_at board pos (Char.lowercase ch);
      let w =
        List.map dirs ~f:(fun dir ->
          let npos = move pos dir in
          let nch = get_at board npos in
          if Char.equal ch nch then fill0 board npos else [])
      in
      pos :: List.concat w)
  in
  fill0 board pos
;;

let fence board area =
  let z =
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
  Tools.sum_of_ints z
;;

let calc_cost board pos =
  let ch = get_at board pos in
  if Char.between ~low:'A' ~high:'Z' ch
  then (
    let area = fill board pos in
    List.length area * fence board area)
  else 0
;;

let read_board = Tools.read_lines () |> List.map ~f:String.to_array |> List.to_array

let () =
  let board = read_board in
  let costs = List.map ~f:(calc_cost board) (coords board) in
  let n = Tools.sum_of_ints costs in
  Stdlib.Printf.printf "Solution 1: %d\n" n
;;
