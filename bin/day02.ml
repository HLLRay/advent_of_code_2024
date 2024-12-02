open Core
open Advent_of_code_2024.Utils

let diffs xs =
  let open List in
  zip_unequal xs (tl xs |> Option.value ~default:[])
  |> map ~f:(Tuple.T2.uncurry ( - ))

let safeDecreasingDiff d = 1 <= d && d <= 3

let part_1_ans =
  let open List in
  let safe (xs : int list) : bool =
    diffs xs
    |> (let sign n = if n <> 0 then n / Int.abs n else 0 in
        fun ds ->
          map ~f:(ds |> hd |> Option.value ~default:0 |> sign |> ( * )) ds)
    |> for_all ~f:safeDecreasingDiff
  in
  read_lines_list "inputs/02-01" int_of_string |> filter ~f:safe |> length

let part_2_ans =
  let open List in
  let decreasing_safe ds =
    let ds_arr = Array.of_list ds in
    filter_mapi ~f:(fun i d -> if safeDecreasingDiff d then None else Some i) ds
    |> function
    | [] -> true
    | [ i ] ->
        i = 0
        || i = length ds - 1
        || safeDecreasingDiff (ds_arr.(i) + ds_arr.(i + 1))
        || safeDecreasingDiff (ds_arr.(i) + ds_arr.(i - 1))
    | [ i; j ] -> i + 1 = j && safeDecreasingDiff (ds_arr.(i) + ds_arr.(j))
    | _ -> false
  in
  let safe (xs : int list) : bool =
    diffs xs |> fun ds ->
    decreasing_safe ds || decreasing_safe (map ~f:(( * ) ~-1) ds)
  in
  read_lines_list "inputs/02-02" int_of_string
  |> filter ~f:(fun xs -> safe xs)
  |> length

let () = print_solutions part_1_ans part_2_ans
