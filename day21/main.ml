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
    let p = if equal_char pad.(r2).(c1) ' ' then [] else [ move_v dr ^ move_h dc ] in
    let p = if equal_char pad.(r1).(c2) ' ' then p else (move_h dc ^ move_v dr) :: p in
    p)
;;

(* let pos_to_str pos = "(" ^ Int.to_string (fst pos) ^  "," ^ Int.to_string (snd pos) ^")" *)
(* let p = paths keypad (3, 2) (1, 0) in
  List.iter p ~f:(fun path ->
    Stdlib.Printf.printf
      "%s\n"
      (List.fold path ~init:"" ~f:(fun acc v -> acc ^ Char.to_string v))); *)

let find_paths pad keys =
  let found = ref [] in
  let pos = find pad 'A' in
  let rec moves pos i acc =
    if i = String.length keys
    then found := acc :: !found
    else (
      let pos2 = find pad (String.get keys i) in
      paths pad pos pos2
      |> List.iter ~f:(fun path -> moves pos2 (i + 1) (acc ^ path ^ "A")))
  in
  moves pos 0 "";
  !found
;;

let shortest pp =
  List.min_elt pp ~compare:(fun a b -> String.length a - String.length b)
  |> Option.value_exn
;;

let enc pad k p =
  let a = find pad p in
  let b = find pad k in
  let pp = paths pad a b in
  let sp = shortest pp in
  (* Stdlib.Printf.printf "enc: %c to %c = %s\n" p k (sp ^ "A"); *)
  sp ^ "A"
;;

(* let last_char s = String.to_list_rev s |> List.hd_exn *)

let rec encode (keys : string) lvl m : string =
  (* Stdlib.Printf.printf "encode: %s (%d)\n" keys lvl; *)
  if lvl = m
  then keys
  else (
    let pad = if lvl = 0 then keypad else dirpad in
    List.fold (String.to_list keys) ~init:("", 'A') ~f:(fun (acc, p) k ->
      let e = enc pad k p in
      let ee = encode e (lvl + 1) m in
      acc ^ ee, k)
    |> fst)
;;

let () =
  let codes = Tools.read_lines () in
  let n =
    List.fold codes ~init:0 ~f:(fun acc code ->
      let pp = encode code 0 3 in
      let v = Int.of_string (String.chop_suffix_if_exists ~suffix:"A" code) in
      Stdlib.Printf.printf
        "%s to %s len=%d %d\n"
        code
        pp
        (String.length pp)
        (v * String.length pp);
      acc + (v * String.length pp))
  in
  Stdlib.Printf.printf "Solution 1: %d\n" n
;;
(* Stdlib.Printf.printf "%s\n" (find_paths dirpad "v" |> String.concat ~sep:"|") *)
