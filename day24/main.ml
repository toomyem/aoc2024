open Base

type gate =
  | BIT of int
  | OR of string * string
  | AND of string * string
  | XOR of string * string

let build_device lines =
  let update d line =
    Stdlib.Printf.printf "%s\n" line;
    if String.contains line ':'
    then (
      let arr = Pcre2.extract line ~full_match:false ~pat:"([a-z0-9]+): (\\d+)" in
      Map.set d ~key:arr.(0) ~data:(BIT (Int.of_string arr.(1))))
    else if String.contains line '>'
    then (
      let arr =
        Pcre2.extract
          line
          ~full_match:false
          ~pat:"([a-z0-9]+) (\\S+) ([a-z0-9]+) -> ([a-z0-9]+)"
      in
      let gate =
        match arr.(1) with
        | "OR" -> OR (arr.(0), arr.(2))
        | "AND" -> AND (arr.(0), arr.(2))
        | "XOR" -> XOR (arr.(0), arr.(2))
        | _ -> failwith ("Invalid operation: " ^ arr.(1))
      in
      Map.set d ~key:arr.(3) ~data:gate)
    else d
  in
  List.fold lines ~init:(Map.empty (module String)) ~f:update
;;

let rec calc d g =
  let g = Map.find_exn d g in
  match g with
  | BIT x -> x
  | OR (x, y) -> calc d x lor calc d y
  | XOR (x, y) -> calc d x lxor calc d y
  | AND (x, y) -> calc d x land calc d y
;;

let () =
  let d = Tools.read_lines () |> build_device in
  let zz =
    Map.keys d
    |> List.filter ~f:(fun g -> String.is_prefix g ~prefix:"z")
    |> List.sort ~compare:String.compare
  in
  let v = List.map zz ~f:(fun g -> calc d g) in
  let n = List.foldi v ~init:0 ~f:(fun i acc v -> acc + ((2 ** i) * v)) in
  Stdlib.Printf.printf "Solution 1: %d\n" n
;;
