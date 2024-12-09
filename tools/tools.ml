open Base
open Stdio

let read_line () = In_channel.input_line stdin |> Option.value_exn

let read_lines () =
  let rec read_lines0 acc =
    try
      let line = read_line () in
      line :: read_lines0 acc
    with
    | _ -> acc
  in
  read_lines0 []
;;

let to_int_list (line : string) : int list =
  Pcre2.extract_all ~full_match:false ~pat:"(\\d+)" line
  |> Array.map ~f:(fun a -> Int.of_string a.(0))
  |> Array.to_list
;;

let rec permute (l : 'a list) : 'a list list =
  let insert a l =
    let len = List.length l in
    List.range ~stop:`inclusive 0 len
    |> List.map ~f:(fun i ->
      List.concat [ List.sub l ~pos:0 ~len:i; [ a ]; List.sub l ~pos:i ~len:(len - i) ])
  in
  match l with
  | [] -> []
  | hd :: tl ->
    let p = permute tl in
    if List.is_empty p then [ [ hd ] ] else p |> List.map ~f:(insert hd) |> List.concat
;;

let sum_of_ints lst = List.fold ~init:0 ~f:(fun acc x -> acc + x) lst

let str_of_ints lst =
  List.fold ~init:"" ~f:(fun acc x -> acc ^ Int.to_string x ^ ",") lst
  |> String.chop_suffix_if_exists ~suffix:","
;;
