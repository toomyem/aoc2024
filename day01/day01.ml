open Base
open Tools

let count el list =
  List.fold ~init:0 ~f:(fun acc x -> if el = x then acc + 1 else acc) list
;;

let () =
  let data =
    read_lines ()
    |> List.fold ~init:([], []) ~f:(fun acc line ->
      Stdlib.Printf.printf "# %s\n" line;
      let arr = Pcre2.extract ~full_match:false ~pat:"(\\d+)\\s+(\\d+)" line in
      let e1 = arr.(0) |> Int.of_string in
      let e2 = arr.(1) |> Int.of_string in
      e1 :: fst acc, e2 :: snd acc)
  in
  let sorted1 = List.sort (fst data) ~compare:Int.compare in
  let sorted2 = List.sort (snd data) ~compare:Int.compare in
  let data = List.map2_exn ~f:(fun a b -> abs (a - b)) sorted1 sorted2 in
  let n1 = List.fold ~init:0 ~f:(fun acc a -> acc + a) data in
  let n2 = List.fold ~init:0 ~f:(fun acc a -> acc + (a * count a sorted2)) sorted1 in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
