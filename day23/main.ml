open Base

let make_graph lines =
  List.map lines ~f:(fun line ->
    let arr = Pcre2.extract ~full_match:false ~pat:"(\\w+)-(\\w+)" line in
    arr.(0), arr.(1))
  |> List.fold
       ~init:(Map.empty (module String))
       ~f:(fun g (n1, n2) ->
         g |> Map.add_multi ~key:n1 ~data:n2 |> Map.add_multi ~key:n2 ~data:n1)
;;

let find_cycles g n =
  Stdlib.Printf.printf "%s\n" n;
  Stdlib.flush Stdio.stdout;
  let cycles = ref [] in
  let rec walk path =
    let exits = Map.find_multi g (List.hd_exn path) in
    List.iter exits ~f:(fun e ->
      if equal_string e n
      then cycles := path :: !cycles
      else if (not (List.exists path ~f:(equal_string e))) && List.length path < 4
      then walk (e :: path))
  in
  walk [ n ];
  !cycles
;;

let count_cycles g =
  Map.keys g
  |> List.filter ~f:(fun n -> String.is_prefix ~prefix:"t" n)
  |> List.map ~f:(fun n -> find_cycles g n)
;;

let () =
  let g = Tools.read_lines () |> make_graph in
  let n = count_cycles g |> List.concat in
  let x =
    n
    |> List.filter ~f:(fun c -> List.length c = 3)
    |> List.map ~f:(fun c ->
      List.sort c ~compare:String.compare |> String.concat ~sep:"-")
    |> List.dedup_and_sort ~compare:String.compare
  in
  Stdlib.Printf.printf "Solution 1: %d\n" (List.length x)
;;
