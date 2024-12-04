open Base

let get puzzle x y : string =
  if x >= 0 && x < Array.length puzzle.(0) && y >= 0 && y < Array.length puzzle
  then String.of_char puzzle.(y).(x)
  else ""
;;

let get_row puzzle i = String.of_array puzzle.(i)
let get_col puzzle i = Array.map ~f:(fun arr -> arr.(i)) puzzle |> String.of_array

let get_l_diag puzzle i =
  let h = Array.length puzzle in
  List.range 0 h |> List.fold ~init:"" ~f:(fun acc x -> acc ^ get puzzle (i - x) x)
;;

let get_r_diag puzzle i =
  let w = Array.length puzzle.(0) in
  let h = Array.length puzzle in
  List.range 0 h |> List.fold ~init:"" ~f:(fun acc x -> acc ^ get puzzle (-w + i + x) x)
;;

let find_xmas str =
  let find str =
    try Pcre2.extract_all ~full_match:false ~pat:"(XMAS)" str |> Array.length with
    | Stdlib.Not_found -> 0
  in
  find str + find (String.rev str)
;;

let is_mas puzzle (x, y) =
  let check x y ch = Char.equal puzzle.(x).(y) ch in
  if check x y 'A'
     && ((check (x - 1) (y - 1) 'M' && check (x + 1) (y + 1) 'S')
         || (check (x - 1) (y - 1) 'S' && check (x + 1) (y + 1) 'M'))
     && ((check (x - 1) (y + 1) 'M' && check (x + 1) (y - 1) 'S')
         || (check (x - 1) (y + 1) 'S' && check (x + 1) (y - 1) 'M'))
  then 1
  else 0
;;

let sum lst = List.fold ~init:0 ~f:(fun acc x -> acc + x) lst

let () =
  let puzzle = Tools.read_lines () |> List.to_array |> Array.map ~f:String.to_array in
  let w = Array.length puzzle.(0) in
  let h = Array.length puzzle in
  let rc =
    List.range 0 w |> List.map ~f:(get_col puzzle) |> List.map ~f:find_xmas |> sum
  in
  let rr =
    List.range 0 h |> List.map ~f:(get_row puzzle) |> List.map ~f:find_xmas |> sum
  in
  let dl =
    List.range 0 (w + h)
    |> List.map ~f:(get_l_diag puzzle)
    |> List.map ~f:find_xmas
    |> sum
  in
  let dr =
    List.range 0 (w + h)
    |> List.map ~f:(get_r_diag puzzle)
    |> List.map ~f:find_xmas
    |> sum
  in
  let total = rc + rr + dl + dr in
  let mas =
    List.cartesian_product (List.range 1 (w - 1)) (List.range 1 (h - 1))
    |> List.map ~f:(is_mas puzzle)
    |> sum
  in
  Stdlib.Printf.printf "Solution 1: %d\n" total;
  Stdlib.Printf.printf "Solution 2: %d\n" mas
;;
