open Base

let coords board =
  let h = Array.length board
  and w = Array.length board.(0) in
  List.cartesian_product (List.range 0 h) (List.range 0 w)
;;

let get_at board pos =
  let r = fst pos
  and c = snd pos in
  let h = Array.length board
  and w = Array.length board.(0) in
  if r >= 0 && r < h && c >= 0 && c < w then board.(r).(c) else '.'
;;

let visit board pos =
  let r = fst pos
  and c = snd pos in
  let ch = board.(r).(c) in
  board.(r).(c) <- Char.lowercase ch
;;

let rec fill board pos =
  let ch = get_at board pos in
  if Char.equal ch '.'
  then []
  else (
    visit board pos;
    let r = fst pos
    and c = snd pos in
    let w =
      List.map
        ~f:(fun (dy, dx) ->
          let npos = r + dy, c + dx in
          let nch = get_at board npos in
          if Char.equal ch nch then fill board npos else [])
        [ 0, 1; 1, 0; -1, 0; 0, -1 ]
    in
    pos :: List.concat w)
;;

let fence board area =
  let z =
    List.map
      ~f:(fun pos ->
        let ch = get_at board pos in
        let r = fst pos
        and c = snd pos in
        let f =
          List.map
            ~f:(fun (dy, dx) ->
              let npos = r + dy, c + dx in
              let nch = get_at board npos in
              if Char.equal ch nch then 1 else 0)
            [ 0, 1; 1, 0; -1, 0; 0, -1 ]
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
    Stdlib.Printf.printf "%d\n" (List.length area);
    List.length area * fence board area)
  else 0
;;

let () =
  let lines = Tools.read_lines () in
  let board = List.to_array lines |> Array.map ~f:String.to_array in
  let costs = List.map ~f:(calc_cost board) (coords board) in
  let n = Tools.sum_of_ints costs in
  Stdlib.Printf.printf "Solution 1: %d\n" n
;;
