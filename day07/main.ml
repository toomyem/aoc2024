open Base

let add a b = a + b
let mul a b = a * b
let con a b = Int.of_string (Int.to_string a ^ Int.to_string b)
let ops1 = [ add; mul ]
let ops2 = [ add; mul; con ]

let calc_variant ops numbers v =
  let o = List.length ops in
  match numbers with
  | first :: rest ->
    List.fold
      ~init:(first, v)
      ~f:(fun (acc, s) n ->
        let f = List.nth_exn ops (s % o) in
        f acc n, s / o)
      rest
    |> fst
  | _ -> failwith "Invalid input"
;;

let calc ops numbers total =
  let n = List.length numbers - 1 in
  let o = List.length ops in
  let found =
    List.range 0 (o ** n) |> List.find ~f:(fun v -> calc_variant ops numbers v = total)
  in
  match found with
  | Some _ -> total
  | None -> 0
;;

let calc_value ops line =
  let v = Tools.to_int_list line in
  match v with
  | total :: numbers -> calc ops numbers total
  | _ -> failwith "Invalid input"
;;

let () =
  let lines = Tools.read_lines () in
  let n1 = lines |> List.map ~f:(calc_value ops1) |> Tools.sum_of_ints in
  let n2 = lines |> List.map ~f:(calc_value ops2) |> Tools.sum_of_ints in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
