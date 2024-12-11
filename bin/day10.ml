open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Graphs
open Advent_of_code_2024.Vectors

let part_1_ans =
  let map =
    read_grid "inputs/10-01" |> Array.map ~f:(Array.map ~f:Char.get_digit_exn)
  in
  let score (i, j) : int =
    let seen = map |> Array.map ~f:(Array.map ~f:(const false)) in
    dfs
      ~neighbours:(fun (i, j) ->
        let current_height = map.(i).(j) in
        orth_neighbours_in_grid map (i, j)
        |> List.filter ~f:(fun (new_i, new_j) ->
               map.(new_i).(new_j) = current_height + 1))
      ~seen:(fun (i, j) -> seen.(i).(j))
      ~mark_seen:(fun (i, j) -> seen.(i).(j) <- true)
      (i, j)
    |> Sequence.count ~f:(fun (i, j) -> map.(i).(j) = 9)
  in
  Array.foldi ~init:0
    ~f:(fun i acc_i ->
      Array.foldi ~init:acc_i ~f:(fun j acc_j height ->
          acc_j + if height = 0 then score (i, j) else 0))
    map

let part_2_ans =
  let map =
    read_grid "inputs/10-02" |> Array.map ~f:(Array.map ~f:Char.get_digit_exn)
  in
  let rating (i, j) : int =
    dfs
      ~neighbours:(fun (i, j) ->
        let current_height = map.(i).(j) in
        orth_neighbours_in_grid map (i, j)
        |> List.filter ~f:(fun (new_i, new_j) ->
               map.(new_i).(new_j) = current_height + 1))
      ~seen:(const false)
      ~mark_seen:(const ())
      (i, j)
    |> Sequence.count ~f:(fun (i, j) -> map.(i).(j) = 9)
  in
  Array.foldi ~init:0
    ~f:(fun i acc_i ->
      Array.foldi ~init:acc_i ~f:(fun j acc_j height ->
          acc_j + if height = 0 then rating (i, j) else 0))
    map

let () = print_solutions part_1_ans part_2_ans
