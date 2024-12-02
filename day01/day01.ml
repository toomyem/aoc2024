open Base

let count el list =
  List.fold ~init:0 ~f:(fun acc x -> if el = x then acc + 1 else acc) list
;;

let () =
  let list1, list2 =
    Tools.read_lines ()
    |> List.map ~f:Tools.to_int_list
    |> List.map ~f:(fun lst -> List.nth_exn lst 0, List.nth_exn lst 1)
    |> List.unzip
  in
  let sorted1 = List.sort list1 ~compare:Int.compare in
  let sorted2 = List.sort list2 ~compare:Int.compare in
  let data = List.map2_exn ~f:(fun a b -> abs (a - b)) sorted1 sorted2 in
  let n1 = List.fold ~init:0 ~f:(fun acc a -> acc + a) data in
  let n2 = List.fold ~init:0 ~f:(fun acc a -> acc + (a * count a sorted2)) sorted1 in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
