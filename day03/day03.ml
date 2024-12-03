open Base

type instr =
  | Mul of int
  | Do
  | Dont

type state =
  { value : int
  ; enabled : bool
  }

let start_state = { value = 0; enabled = true }

let calc_mul (exp : string) =
  Stdlib.Printf.printf "%s\n" exp;
  let arr = Pcre2.extract ~full_match:false ~pat:"(\\d+),(\\d+)" exp in
  Int.of_string arr.(0) * Int.of_string arr.(1)
;;

let parse_instr exp =
  match String.prefix exp 3 with
  | "mul" -> Mul (calc_mul exp)
  | "do(" -> Do
  | "don" -> Dont
  | _ -> failwith "Invalid instruction"
;;

let update_state state (ins : instr) =
  match ins with
  | Do -> { state with enabled = true }
  | Dont -> { state with enabled = false }
  | Mul v when state.enabled -> { state with value = state.value + v }
  | Mul _ -> state
;;

let calc_value instr =
  let state = instr |> List.fold ~init:start_state ~f:update_state in
  state.value
;;

let keep_mul_only instr =
  List.filter
    ~f:(fun i ->
      match i with
      | Mul _ -> true
      | _ -> false)
    instr
;;

let () =
  let instr =
    Tools.read_lines ()
    |> String.concat ~sep:" "
    |> Pcre2.extract_all
         ~full_match:false
         ~pat:"(mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\))"
    |> Array.map ~f:(fun e -> parse_instr e.(0))
    |> Array.to_list
  in
  let n1 = instr |> keep_mul_only |> calc_value in
  let n2 = instr |> calc_value in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
