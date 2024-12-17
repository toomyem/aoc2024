open Base

type state =
  { a : int
  ; b : int
  ; c : int
  ; p : int list
  }

let empty_state = { a = 0; b = 0; c = 0; p = [] }

let update_state state line =
  let get_all () = Tools.to_int_list line in
  let get_first () = get_all () |> List.hd_exn in
  if String.is_prefix line ~prefix:"Register A"
  then { state with a = get_first () }
  else if String.is_prefix line ~prefix:"Register B"
  then { state with b = get_first () }
  else if String.is_prefix line ~prefix:"Register C"
  then { state with c = get_first () }
  else if String.is_prefix line ~prefix:"Program"
  then { state with p = get_all () }
  else state
;;

let run (state : state) : int list =
  let a = ref state.a in
  let b = ref state.b in
  let c = ref state.c in
  let pc = ref 0 in
  let out = ref [] in
  let p = List.to_array state.p in
  let combo op =
    match op with
    | 0 | 1 | 2 | 3 -> op
    | 4 -> !a
    | 5 -> !b
    | 6 -> !c
    | _ -> failwith "Invalid combo"
  in
  while !pc < Array.length p do
    let op = p.(!pc + 1) in
    match p.(!pc) with
    | 0 ->
      a := !a / (2 ** combo op);
      pc := !pc + 2
    | 1 ->
      b := !b lxor op;
      pc := !pc + 2
    | 2 ->
      b := combo op % 8;
      pc := !pc + 2
    | 3 -> if !a <> 0 then pc := op else pc := !pc + 2
    | 4 ->
      b := !b lxor !c;
      pc := !pc + 2
    | 5 ->
      out := !out @ [ combo op % 8 ];
      pc := !pc + 2
    | 6 ->
      b := !a / (2 ** combo op);
      pc := !pc + 2
    | 7 ->
      c := !a / (2 ** combo op);
      pc := !pc + 2
    | _ -> failwith "Invalid instruction"
  done;
  !out
;;

exception Break

let matching l1 l2 =
  List.zip_exn l1 l2
  |> List.rev
  |> List.take_while ~f:(fun (a, b) -> a = b)
  |> List.length
;;

let find_a state =
  let s = List.length state.p in
  let arr = Array.init s ~f:(fun i -> if i < s - 1 then 0 else 1) in
  let i = ref (Array.length arr - 1) in
  let a = ref 0 in
  let stop = ref false in
  while not !stop do
    a := Array.foldi ~init:0 ~f:(fun i acc n -> acc + ((8 ** i) * n)) arr;
    let out = run { state with a = !a } in
    Stdlib.flush Stdio.stdout;
    let m = matching out state.p in
    if m = Array.length arr then stop := true else i := s - 1 - m;
    try
      while !i < Array.length arr do
        let n = arr.(!i) + 1 in
        if n < 8
        then (
          arr.(!i) <- n;
          raise Break)
        else (
          arr.(!i) <- 0;
          i := !i + 1)
      done
    with
    | Break -> ()
  done;
  !a
;;

let () =
  let state = Tools.read_lines () |> List.fold ~init:empty_state ~f:update_state in
  let out = run state in
  Stdlib.Printf.printf "Solution 1: %s\n" (Tools.str_of_ints out);
  let a = find_a state in
  Stdlib.Printf.printf "Solution 2: %d\n" a
;;
