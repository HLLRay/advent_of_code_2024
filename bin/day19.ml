open Core
open Advent_of_code_2024.Utils

module CharList = struct
  module T = struct
    type t = char list

    let compare x y = String.compare (String.of_list x) (String.of_list y)
    let sexp_of_t = sexp_of_list Char.sexp_of_t
    let t_of_sexp = list_of_sexp Char.t_of_sexp
    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make_and_derive_hash_fold_t (T)
end

let read_input filename =
  let channel = In_channel.create filename in
  let lines = In_channel.input_lines channel in
  let towels =
    List.hd_exn lines
    |> String.split_on_chars ~on:[ ',' ]
    |> List.map ~f:(String.strip >> String.to_list >> fun ls -> (ls, 1))
  in
  let patterns =
    List.drop lines 2 |> List.map ~f:(String.strip >> String.to_list)
  in
  (towels, patterns)

let rec consume pattern towel =
  match (pattern, towel) with
  | ps, [] -> Some ps
  | p :: ps, t :: ts when p = t -> consume ps ts
  | _ -> None

let num_arrangements towels =
  let num_arrangements_no_recurse f = function
    | [] -> 1
    | pattern ->
        towels
        |> List.filter_map ~f:(fun (towel, multiplier) ->
               consume pattern towel
               |> Option.map ~f:(fun pattern -> (pattern, multiplier)))
        |> List.fold ~init:0 ~f:(fun acc (pattern, multiplier) ->
               acc + (multiplier * f pattern))
  in
  Memo.recursive ~hashable:CharList.hashable num_arrangements_no_recurse

let simplify_towels towels =
  let rec simplify_towels_inner done_towels = function
    | [] -> done_towels
    | (towel, n) :: rest ->
        if num_arrangements (done_towels @ rest) towel > 0 then
          simplify_towels_inner done_towels rest
        else simplify_towels_inner ((towel, n) :: done_towels) rest
  in
  simplify_towels_inner towels []

let num_arrangements_by_pattern =
  let towels, patterns = read_input "inputs/19-01" in
  let towels = simplify_towels towels in
  List.map patterns ~f:(num_arrangements towels)

let part_1_ans = List.count num_arrangements_by_pattern ~f:(( < ) 0)
let part_2_ans = List.sum (module Int) num_arrangements_by_pattern ~f:Fn.id
let () = print_solutions part_1_ans part_2_ans
