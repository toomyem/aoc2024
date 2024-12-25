open Base

let keypad =
  [| [| '7'; '8'; '9' |]; [| '4'; '5'; '6' |]; [| '1'; '2'; '3' |]; [| ' '; '0'; 'A' |] |]
;;

let dirpad = [| [| ' '; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let find pad ch =
  let h = Array.length pad in
  let w = Array.length pad.(0) in
  List.cartesian_product (List.range 0 h) (List.range 0 w)
  |> List.find_exn ~f:(fun (r, c) -> equal_char pad.(r).(c) ch)
;;

let move_h d =
  if d < 0 then String.make (-d) '<' else if d > 0 then String.make d '>' else ""
;;

let move_v d =
  if d < 0 then String.make (-d) '^' else if d > 0 then String.make d 'v' else ""
;;

let paths pad (r1, c1) (r2, c2) =
  let dr, dc = r2 - r1, c2 - c1 in
  if dr = 0 && dc = 0
  then [ "" ]
  else if dr = 0
  then [ move_h dc ]
  else if dc = 0
  then [ move_v dr ]
  else (
    let p = if equal_char pad.(r1).(c2) ' ' then [] else [ move_h dc ^ move_v dr ] in
    let p = if equal_char pad.(r2).(c1) ' ' then p else (move_v dr ^ move_h dc) :: p in
    p)
;;

let shortest pp =
  List.min_elt pp ~compare:(fun a b -> String.length a - String.length b)
  |> Option.value_exn
;;

let enc pad k p =
  let a = find pad p in
  let b = find pad k in
  let pp = paths pad a b in
  List.map pp ~f:(fun s -> s ^ "A")
;;

(* List.map pp ~f:(fun p -> p ^ "A") *)

(* let last_char s = String.to_list_rev s |> List.hd_exn *)

let h = Hashtbl.create (module String)

let rec encode (keys : string) lvl m : string =
  Stdlib.Printf.printf "encode: %s (lvl=%d)\n" keys lvl;
  Stdlib.flush Stdio.stdout;
  match Hashtbl.find h (keys ^ Int.to_string lvl) with
  | Some x -> x
  | None ->
    let x =
      if lvl = m
      then keys
      else (
        let pad = if lvl = 0 then keypad else dirpad in
        String.fold keys ~init:("", 'A') ~f:(fun (acc, p) k ->
          let e = enc pad k p in
          let ee = List.map e ~f:(fun e -> encode e (lvl + 1) m) |> shortest in
          acc ^ ee, k)
        |> fst)
    in
    Hashtbl.set h ~key:(keys ^ Int.to_string lvl) ~data:x;
    x
;;

let () =
  let codes = Tools.read_lines () in
  let n =
    List.fold codes ~init:0 ~f:(fun acc code ->
      let p = encode code 0 21 in
      let v = Int.of_string (String.chop_suffix_if_exists ~suffix:"A" code) in
      Stdlib.Printf.printf
        "%s %s len=%dMB %d\n"
        code
        (String.prefix p 80)
        (String.length p / 1024 / 1024)
        (v * String.length p);
      Stdlib.flush Stdio.stdout;
      acc + (v * String.length p))
  in
  Stdlib.Printf.printf "Solution 1: %d\n" n
;;
(* Stdlib.Printf.printf "%s\n" (find_paths dirpad "v" |> String.concat ~sep:"|") *)
