open Base

let blink n =
  let s = Int.to_string n in
  let l = String.length s in
  if n = 0
  then [ 1 ]
  else if l % 2 = 0
  then
    [ String.prefix s (l / 2) |> Int.of_string; String.suffix s (l / 2) |> Int.of_string ]
  else [ n * 2024 ]
;;

let split n steps =
  let h = Hashtbl.create (module String) in
  let rec split0 n steps =
    let key = Int.to_string n ^ "/" ^ Int.to_string steps in
    if steps = 0
    then 1
    else if Hashtbl.mem h key
    then Hashtbl.find_exn h key
    else (
      let k =
        match blink n with
        | [ n1 ] -> split0 n1 (steps - 1)
        | [ n1; n2 ] -> split0 n1 (steps - 1) + split0 n2 (steps - 1)
        | _ -> failwith "Sth went really wrong"
      in
      Hashtbl.set h ~key ~data:k;
      k)
  in
  split0 n steps
;;

let () =
  let numbers = Tools.read_line () |> Tools.to_int_list in
  let n1 = List.map ~f:(fun n -> split n 25) numbers |> Tools.sum_of_ints in
  let n2 = List.map ~f:(fun n -> split n 75) numbers |> Tools.sum_of_ints in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
