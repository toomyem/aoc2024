open Base

type file =
  { start : int
  ; size : int
  }

type phase =
  | File
  | Free

(* prepare an array representing disks' blocks *)
let make_array line : int array =
  let rec mk acc id ph (data : int list) =
    match data, ph with
    | n :: hd, File -> mk (Array.append acc (Array.create ~len:n id)) (id + 1) Free hd
    | n :: hd, Free -> mk (Array.append acc (Array.create ~len:n (-1))) id File hd
    | [], _ -> acc
  in
  mk [||] 0 File (String.to_list line |> List.map ~f:Char.get_digit_exn)
;;

let checksum arr =
  arr
  |> Array.mapi ~f:(fun i n ->
    match n with
    | -1 -> 0
    | n -> i * n)
  |> Array.to_list
  |> Tools.sum_of_ints
;;

let find_file max arr i : file option =
  let i = ref (i - 1) in
  while !i >= 0 && arr.(!i) = -1 do
    i := !i - 1
  done;
  if !i < 0
  then None
  else (
    let e = !i in
    while !i >= 0 && arr.(!i) = arr.(e) && e - !i < max do
      i := !i - 1
    done;
    Some { start = !i + 1; size = e - !i })
;;

(* check is arr has free space of size starting from i *)
let rec is_free arr i size =
  if size = 0
  then true
  else if i >= Array.length arr || arr.(i) <> -1
  then false
  else is_free arr (i + 1) (size - 1)
;;

let find_free arr size : int option =
  let i = ref 0 in
  while !i < Array.length arr && not (is_free arr !i size) do
    i := !i + 1
  done;
  if !i = Array.length arr then None else Some !i
;;

let move arr f i =
  for k = 0 to f.size - 1 do
    arr.(i + k) <- arr.(f.start + k);
    arr.(f.start + k) <- -1
  done
;;

(* defragment array in place handling files of max size *)
let defragment max arr : unit =
  let rec defr arr j =
    match find_file max arr j with
    | None -> ()
    | Some f ->
      (match find_free arr f.size with
       | None -> defr arr f.start
       | Some i ->
         if i < f.start then move arr f i;
         defr arr f.start;
         ())
  in
  defr arr (Array.length arr)
;;

let () =
  let line = Tools.read_line () in
  let arr1 = make_array line in
  defragment 1 arr1;
  let n1 = checksum arr1 in
  let arr2 = make_array line in
  defragment 10 arr2;
  let n2 = checksum arr2 in
  Stdlib.Printf.printf "Solution 1: %d\n" n1;
  Stdlib.Printf.printf "Solution 2: %d\n" n2
;;
