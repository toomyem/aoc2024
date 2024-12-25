open Base

let is_key l = String.for_all (List.hd_exn l) ~f:(fun ch -> equal_char ch '#')

let sum_columns l =
  List.init
    (String.length (List.hd_exn l))
    ~f:(fun i ->
      (List.map l ~f:(fun r ->
         match String.get r i with
         | '#' -> 1
         | _ -> 0)
       |> Tools.sum_of_ints)
      - 1)
;;

let read_all () =
  Tools.read_lines ()
  |> List.fold ~init:([], [], []) ~f:(fun (acc, keys, holes) line ->
    if String.is_empty line
    then
      if is_key acc
      then [], sum_columns acc :: keys, holes
      else [], keys, sum_columns acc :: holes
    else line :: acc, keys, holes)
;;

let () =
  let _, keys, holes = read_all () in
  let n =
    List.cartesian_product keys holes
    |> List.count ~f:(fun (k, h) ->
      let i = List.zip_exn k h |> List.for_all ~f:(fun (a, b) -> a + b <= 5) in
      Stdlib.Printf.printf "%s\n%s\n%b\n\n" (Tools.str_of_ints k) (Tools.str_of_ints h) i;
      i)
  in
  Stdlib.Printf.printf "Solution 1: %d\n" n;
  Stdlib.Printf.printf "total: %d %d\n" (List.length keys) (List.length holes)
;;
