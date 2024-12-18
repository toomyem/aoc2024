open Base
open Tools

let all_dirs = [ 0; 1; 2; 3 ]

let go dir (r, c) =
  match dir with
  | 0 -> r - 1, c
  | 1 -> r, c + 1
  | 2 -> r + 1, c
  | 3 -> r, c - 1
  | d -> failwith ("Invalid dir " ^ Int.to_string d)
;;

module Pos = struct
  type t = int * int

  let compare a b =
    match Int.compare (fst a) (fst b) with
    | 0 -> Int.compare (snd a) (snd b)
    | n -> n
  ;;

  let hash a =
    hash_fold_int (hash_fold_int (Hash.create ()) (fst a)) (snd a) |> Hash.get_hash_value
  ;;

  let sexp_of_t a = Sexp.List [ Int.sexp_of_t (fst a); Int.sexp_of_t (snd a) ]
end

type cell =
  | Wall
  | Empty
  | Visited of int

type memory = (Pos.t, cell) Hashtbl.t

let to_memory pos_list : memory =
  let mem = Hashtbl.create (module Pos) in
  let update pos = Hashtbl.add_exn mem ~key:pos ~data:Wall in
  List.iter ~f:update pos_list;
  mem
;;

let same_pos a b = Pos.compare a b = 0

let flood w h (m : memory) =
  let sp = 0, 0
  and ep = h - 1, w - 1 in
  let get m ((r, c) as p) =
    if r >= 0 && c >= 0 && r < h && c < w
    then (
      match Hashtbl.find m p with
      | None -> Empty
      | Some v -> v)
    else Wall
  in
  let put m p v = Hashtbl.add_exn m ~key:p ~data:v in
  let q = Queue.singleton ep in
  let len = ref None in
  put m ep (Visited 0);
  while Option.is_none !len && not (Queue.is_empty q) do
    let p = Queue.dequeue_exn q in
    let v =
      match get m p with
      | Visited v -> v
      | _ -> failwith "Invalid state"
    in
    if same_pos p sp
    then len := Some v
    else
      List.iter all_dirs ~f:(fun d ->
        let np = go d p in
        match get m np with
        | Empty ->
          Queue.enqueue q np;
          put m np (Visited (v + 1))
        | _ -> ())
  done;
  !len
;;

let str_to_pos line =
  let arr = to_int_arr line in
  arr.(1), arr.(0)
;;

let pos_to_str p = Int.to_string (snd p) ^ "," ^ Int.to_string (fst p)

let () =
  let pos_list = Tools.read_lines () |> List.map ~f:str_to_pos in
  let memory = List.take pos_list 1024 |> to_memory in
  let n = flood 71 71 memory |> Option.value_exn in
  Stdlib.Printf.printf "Solution 1: %d\n" n;
  let p =
    List.fold_until
      pos_list
      ~init:[]
      ~f:(fun pos_list pos ->
        let pos_list = pos :: pos_list in
        let memory = to_memory pos_list in
        match flood 71 71 memory with
        | None -> Stop pos
        | Some _ -> Continue pos_list)
      ~finish:(fun _ -> 0, 0)
  in
  Stdlib.Printf.printf "Solution 2: %s\n" (pos_to_str p)
;;
