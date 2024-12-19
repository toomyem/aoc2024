open Base
open Tools

let count_arrangements towel strips =
  let h = Hashtbl.create (module String) in
  let strip prefix str = String.chop_prefix_if_exists ~prefix str in
  let prefix prefix str = String.is_prefix ~prefix str in
  let rec count towel =
    match Hashtbl.find h towel with
    | Some v -> v
    | None ->
      let v =
        if String.is_empty towel
        then 1
        else
          List.map
            ~f:(fun s -> if prefix s towel then count (strip s towel) else 0)
            strips
          |> sum_of_ints
      in
      Hashtbl.add_exn h ~key:towel ~data:v;
      v
  in
  count towel
;;

let read_strips () =
  read_line ()
  |> Pcre2.extract_all ~full_match:false ~pat:"([a-z]+)"
  |> Array.map ~f:(fun i -> i.(0))
  |> Array.to_list
;;

let () =
  let strips = read_strips () in
  let _ =
    (* skip empty line *)
    read_line ()
  in
  let towels = read_lines () in
  let n1 = towels |> List.count ~f:(fun t -> count_arrangements t strips > 0) in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  let n2 = towels |> List.map ~f:(fun t -> count_arrangements t strips) |> sum_of_ints in
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
