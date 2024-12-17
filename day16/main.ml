open Base
open Tools

let all_dirs = [ 0; 1; 2; 3 ]

module Pos = struct
  type t = int * int

  let compare a b =
    match Int.compare (fst a) (fst b) with
    | 0 -> Int.compare (snd a) (snd b)
    | n -> n
  ;;

  let sexp_of_t a = Sexp.List [ Int.sexp_of_t (fst a); Int.sexp_of_t (snd a) ]
end

module PosComparator = struct
  include Pos
  include Comparator.Make (Pos)
end

let go dir (r, c) =
  match dir with
  | 0 -> r - 1, c
  | 1 -> r, c + 1
  | 2 -> r + 1, c
  | 3 -> r, c - 1
  | d -> failwith ("Invalid dir " ^ Int.to_string d)
;;

let all = ref []

let update_best best cost path =
  if cost < !best
  then (
    best := cost;
    Stdlib.Printf.printf "Solution 1: %d\n" !best;
    Stdlib.flush Stdio.stdout;
    all := path)
  else if cost = !best
  then (
    all := List.dedup_and_sort (!all @ path) ~compare:Pos.compare;
    Stdlib.Printf.printf "Solution 2: %d\n" (List.length !all);
    Stdlib.flush Stdio.stdout)
;;

let pos_to_str (r, c) = "(" ^ Int.to_string r ^ "," ^ Int.to_string c ^ ")"

let is_node board pos =
  match get_at board pos with
  | 'S' | 'E' -> true
  | '.' -> List.count ~f:(fun dir -> is_at board (go dir pos) '.') all_dirs >= 3
  | _ -> false
;;

let exits board pos = List.filter ~f:(fun d -> is_at board (go d pos) '#' |> not) all_dirs

type edge =
  { p : Pos.t
  ; d : int
  ; c : int
  ; cells : Pos.t list
  }

let build_graph board =
  let rec walk pos dir cost cells =
    let ch = get_at board pos in
    set_at board pos '#';
    let npos = go dir pos in
    let r =
      if is_at board npos '#'
      then None
      else if is_at board npos 'E' || is_at board npos 'S'
      then Some { p = npos; d = dir; c = cost + 1; cells = npos :: cells }
      else (
        match exits board npos with
        | [ d ] when d = dir -> walk npos d (cost + 1) (npos :: cells)
        | [ d ] -> walk npos d (cost + 1001) (npos :: cells)
        | [] -> None
        | _ -> Some { p = npos; d = dir; c = cost + 1; cells })
    in
    set_at board pos ch;
    r
  in
  let build_edges g pos =
    let n = List.map ~f:(fun dir -> walk pos dir 0 [ pos ]) all_dirs in
    Map.set g ~key:pos ~data:n
  in
  let nodes = filter_board board ~f:(is_node board) in
  List.fold ~init:(Map.empty (module PosComparator)) ~f:build_edges nodes
;;

let _pp key data =
  pos_to_str key
  ^ List.fold data ~init:"" ~f:(fun acc v ->
    acc
    ^ "|"
    ^
    match v with
    | Some e -> Stdlib.Printf.sprintf "%s d:%d c:%d" (pos_to_str e.p) e.d e.c
    | None -> "None")
;;

let same_pos p1 p2 = Pos.compare p1 p2 = 0

let find_best g start_pos end_pos =
  let best = ref Int.max_value in
  let rec walk pos dir cost v path =
    if same_pos pos end_pos
    then update_best best cost path
    else if Set.mem v pos
    then ()
    else if cost > !best
    then ()
    else
      Map.find_exn g pos
      |> List.iteri ~f:(fun d e ->
        match e with
        | None -> ()
        | Some e ->
          walk
            e.p
            e.d
            (cost + e.c + if dir = d then 0 else 1000)
            (Set.add v pos)
            (path @ e.cells))
  in
  let visited = Set.empty (module PosComparator) in
  walk start_pos 1 0 visited [];
  !best
;;

let () =
  let board = read_board () in
  let start_pos = find_on_board board 'S' |> Option.value_exn in
  let end_pos = find_on_board board 'E' |> Option.value_exn in
  let graph = build_graph board in
  let n = find_best graph start_pos end_pos in
  Stdlib.Printf.printf "Solution 1: %d\n" n
;;
