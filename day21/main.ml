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

let shortest pp = List.min_elt pp ~compare:Int.compare |> Option.value_exn

let enc pad k p =
  let a = find pad p in
  let b = find pad k in
  let pp = paths pad a b in
  List.map pp ~f:(fun s -> s ^ "A")
;;

let int_value code = Int.of_string (String.chop_suffix_if_exists ~suffix:"A" code)

let solve codes m =
  let h = Hashtbl.create (module String) in
  let rec encode (keys : string) lvl m : int =
    match Hashtbl.find h (keys ^ Int.to_string lvl) with
    | Some x -> x
    | None ->
      let x =
        if lvl = m
        then String.length keys
        else (
          let pad = if lvl = 0 then keypad else dirpad in
          String.fold keys ~init:(0, 'A') ~f:(fun (acc, p) k ->
            let e = enc pad k p in
            let ee = List.map e ~f:(fun e -> encode e (lvl + 1) m) |> shortest in
            acc + ee, k)
          |> fst)
      in
      Hashtbl.set h ~key:(keys ^ Int.to_string lvl) ~data:x;
      x
  in
  List.fold codes ~init:0 ~f:(fun acc code ->
    let n = encode code 0 m in
    let v = int_value code in
    acc + (n * v))
;;

let () =
  let codes = Tools.read_lines () in
  let n1 = solve codes 3 in
  let n2 = solve codes 26 in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
