open Base

type delta =
  { x : int
  ; y : int
  }

type machine =
  { a : delta
  ; b : delta
  ; p : delta
  }

type state =
  { a : delta
  ; b : delta
  ; machines : machine list
  }

let solve machine =
  let { a; b; p } = machine in
  let m = ((p.y * a.x) - (p.x * a.y)) / ((b.y * a.x) - (b.x * a.y)) in
  let n = (p.x - (m * b.x)) / a.x in
  if (n * a.x) + (m * b.x) = p.x && (n * a.y) + (m * b.y) = p.y then (3 * n) + m else 0
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
      machines = { a = state.a; b = state.b; p = get_delta line } :: state.machines
    }
  else state
;;

let read_machines =
  let state =
    Tools.read_lines ()
    |> List.fold
         ~init:{ a = { x = 0; y = 0 }; b = { x = 0; y = 0 }; machines = [] }
         ~f:update_state
  in
  state.machines
;;

let add_offset offset machine =
  { machine with p = { x = machine.p.x + offset; y = machine.p.y + offset } }
;;

let () =
  let machines = read_machines in
  let n1 = machines |> List.map ~f:solve |> Tools.sum_of_ints in
  let n2 =
    machines
    |> List.map ~f:(add_offset 10_000_000_000_000)
    |> List.map ~f:solve
    |> Tools.sum_of_ints
  in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
