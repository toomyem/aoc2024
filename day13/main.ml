open Base

type delta =
  { x : int
  ; y : int
  }

type machine =
  { ba : delta
  ; bb : delta
  ; prize : delta
  }

type state =
  { a : delta
  ; b : delta
  ; machines : machine list
  }

let up_to n = List.range 0 n

let solve machine =
  let { ba; bb; prize } = machine in
  List.cartesian_product (up_to 100) (up_to 100)
  |> List.map ~f:(fun (i, j) ->
    let xx = (i * ba.x) + (j * bb.x) in
    let yy = (i * ba.y) + (j * bb.y) in
    if xx = prize.x && yy = prize.y then (3 * i) + j else -1)
  |> List.filter ~f:(fun x -> x <> -1)
  |> List.min_elt ~compare:Int.compare
  |> Option.value ~default:(-1)
;;

let get_delta line =
  let i = Tools.to_int_list line in
  { x = List.nth_exn i 0; y = List.nth_exn i 1 }
;;

let update_state state line =
  let is_sub s = String.is_substring line ~substring:s in
  if is_sub "Button A"
  then { state with a = get_delta line }
  else if is_sub "Button B"
  then { state with b = get_delta line }
  else if is_sub "Prize"
  then
    { state with
      machines = { ba = state.a; bb = state.b; prize = get_delta line } :: state.machines
    }
  else state
;;

let read_state =
  Tools.read_lines ()
  |> List.fold
       ~init:{ a = { x = 0; y = 0 }; b = { x = 0; y = 0 }; machines = [] }
       ~f:update_state
;;

let () =
  let state = read_state in
  let n =
    state.machines
    |> List.map ~f:solve
    |> List.filter ~f:(fun x -> x <> -1)
    |> Tools.sum_of_ints
  in
  Stdlib.Printf.printf "Solution 1: %d\n" n
;;
