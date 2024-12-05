open Base

type state =
  { ordering : (string, String.comparator_witness) Set.t
  ; updates : int list list
  }

let empty_state = { ordering = Set.empty (module String); updates = [] }
let make_pair x y = Int.to_string x ^ "," ^ Int.to_string y
let ints_of_string line = String.split ~on:',' line |> List.map ~f:Int.of_string

let update_state state line =
  if String.contains line '|'
  then (
    let p = String.split ~on:'|' line |> List.map ~f:Int.of_string in
    let x, y = List.nth_exn p 0, List.nth_exn p 1 in
    { state with ordering = Set.add state.ordering (make_pair x y) })
  else if String.contains line ','
  then (
    let update = ints_of_string line in
    { state with updates = update :: state.updates })
  else state
;;

let cmp ordering x y =
  if Set.mem ordering (make_pair x y)
  then -1
  else if Set.mem ordering (make_pair y x)
  then 1
  else 0
;;

let is_correct ordering pages = List.is_sorted ~compare:(cmp ordering) pages

let fix_order ordering updates =
  List.map ~f:(fun pages -> List.sort ~compare:(cmp ordering) pages) updates
;;

let get_middle lst = List.nth_exn lst (List.length lst / 2)
let sum_middles lst = List.map ~f:get_middle lst |> Tools.sum_of_ints

let () =
  let state = Tools.read_lines () |> List.fold ~init:empty_state ~f:update_state in
  let ordering = state.ordering in
  let correct, incorrect = List.partition_tf ~f:(is_correct ordering) state.updates in
  let n1 = correct |> sum_middles in
  let n2 = incorrect |> fix_order ordering |> sum_middles in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
