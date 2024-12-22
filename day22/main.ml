open Base

let next s =
  let s = s lxor (s * 64) % 16777216 in
  let s = s lxor (s / 32) % 16777216 in
  let s = s lxor (s * 2048) % 16777216 in
  s
;;

let numbers n = Sequence.unfold ~init:n ~f:(fun n -> Some (n, next n))

let fold secret =
  Sequence.unfold_step ~init:(secret, []) ~f:(fun (prev, four) ->
    let n = next prev in
    let diff = (n % 10) - (prev % 10) in
    let s = List.rev four |> Tools.str_of_ints in
    let nfour = List.take (diff :: four) 4 in
    if List.length four < 4
    then Sequence.Step.Skip { state = n, nfour }
    else Sequence.Step.Yield { value = prev % 10, s; state = n, nfour })
;;

let m = ref 0

let score i (secrets : (int * string) list list) s : int =
  let v =
    List.map secrets ~f:(fun lst -> List.find lst ~f:(fun i -> equal_string (snd i) s))
  in
  let v2 =
    v |> List.filter ~f:Option.is_some |> List.map ~f:(fun x -> fst (Option.value_exn x))
  in
  let n = Tools.sum_of_ints v2 in
  if n > !m then m := n;
  Stdlib.Printf.printf "%d. %s - %d (%d)\n" i s n !m;
  Stdlib.flush Stdio.stdout;
  n
;;

let calc secrets : int =
  let v =
    List.map secrets ~f:(fun s -> Sequence.take (fold s) 1996 |> Sequence.to_list)
  in
  let fours =
    v |> List.concat |> List.map ~f:snd |> List.dedup_and_sort ~compare:String.compare
  in
  fours
  |> List.mapi ~f:(fun i s -> score i v s)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let () =
  let secrets = Tools.read_lines () |> List.map ~f:Int.of_string in
  let n1 =
    List.map secrets ~f:(fun s -> Sequence.nth_exn (numbers s) 2000) |> Tools.sum_of_ints
  in
  let n2 = calc secrets in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
