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

let paths pad (r1, c1) (r2, c2) =
  let found = ref [] in
  let rec walk (r, c) acc =
    if r = r2 && c = c2
    then found := acc :: !found
    else if not (equal_char pad.(r).(c) ' ')
    then (
      if c < c2 then walk (r, c + 1) (acc ^ ">");
      if c > c2 then walk (r, c - 1) (acc ^ "<");
      if r < r2 then walk (r + 1, c) (acc ^ "v");
      if r > r2 then walk (r - 1, c) (acc ^ "^"))
  in
  walk (r1, c1) "";
  !found
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

let decode pads code =
  let found = ref [] in
  Stdlib.Printf.printf "code: %s\n" code;
  Stdlib.flush Stdio.stdout;
  let rec dec pads keys =
    match pads with
    | pad :: rest -> find_paths pad keys |> List.iter ~f:(fun path -> dec rest path)
    | [] -> found := keys :: !found
  in
  dec pads code;
  !found
;;

let () =
  let codes = Tools.read_lines () in
  let n =
    List.fold codes ~init:0 ~f:(fun acc code ->
      let pp = decode [ keypad; dirpad; dirpad ] code in
      (* List.iter pp ~f:(fun p -> Stdlib.Printf.printf "%s\n" p); *)
      let m =
        List.map pp ~f:String.length
        |> List.min_elt ~compare:Int.compare
        |> Option.value_exn
      in
      let v = Int.of_string (String.chop_suffix_if_exists ~suffix:"A" code) in
      acc + (v * m))
  in
  Stdlib.Printf.printf "Solution 1: %d\n" n
;;
