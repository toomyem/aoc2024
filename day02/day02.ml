open Base
open Tools

let to_int_list (line : string) : int list =
  Pcre2.extract_all ~full_match:false ~pat:"(\\d+)" line
  |> Array.map ~f:(fun a -> Int.of_string a.(0))
  |> Array.to_list
;;

let remove_at_index index lst =
  let rec remove acc i lst =
    match lst with
    | [] -> acc
    | _ :: tl when i = index -> remove acc (i + 1) tl
    | hd :: tl -> remove (hd :: acc) (i + 1) tl
  in
  remove [] 0 lst |> List.rev
;;

type kind =
  | Start
  | Unknown of int
  | Increasing of int
  | Decreasing of int

let is_strictly_valid (list : int list) =
  let is_inc prev next = prev < next && next - prev <= 3 in
  let is_dec prev next = prev > next && prev - next <= 3 in
  List.fold_until
    ~init:Start
    ~f:(fun acc next ->
      match acc with
      | Increasing prev ->
        if is_inc prev next then Continue (Increasing next) else Stop false
      | Decreasing prev ->
        if is_dec prev next then Continue (Decreasing next) else Stop false
      | Unknown prev ->
        if is_inc prev next
        then Continue (Increasing next)
        else if is_dec prev next
        then Continue (Decreasing next)
        else Stop false
      | Start -> Continue (Unknown next))
    ~finish:(fun _ -> true)
    list
;;

let is_valid (list : int list) =
  if is_strictly_valid list
  then true
  else (
    let not_valid =
      List.for_alli
        ~f:(fun i _ -> list |> remove_at_index i |> is_strictly_valid |> not)
        list
    in
    not not_valid)
;;

let () =
  let data = read_lines () |> List.map ~f:to_int_list in
  let n1 = data |> List.filter ~f:is_strictly_valid |> List.length in
  let n2 = data |> List.filter ~f:is_valid |> List.length in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
