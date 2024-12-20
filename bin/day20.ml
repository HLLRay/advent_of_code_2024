open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Graphs
open Advent_of_code_2024.Vectors

let wall = -1
let unexplored = -2
let explored = -3
let not_wall grid (i, j) = grid.(i).(j) <> wall

let grid, track =
  let char_grid = read_grid "inputs/20-01" in
  let grid =
    char_grid
    |> Array.map ~f:(Array.map ~f:(function '#' -> wall | _ -> unexplored))
  in
  let start_pos = find_last_f ~f:(( = ) 'S') char_grid in
  (* Use dfs here because there is only a single track.
    We also assume that end is at the end of the track.*)
  let track =
    dfs
      ~neighbours:
        (orth_neighbours_in_grid grid >> List.filter ~f:(not_wall grid))
      ~seen:(fun (i, j) -> grid.(i).(j) <> unexplored)
      ~mark_seen:(fun (i, j) -> grid.(i).(j) <- explored)
      start_pos
    |> Sequence.to_list
  in
  List.iteri track ~f:(fun num (i, j) -> grid.(i).(j) <- num);
  (grid, track)

let solve cheat_limit =
  let possible_movements =
    List.init (cheat_limit + 1) ~f:(fun i ->
        List.init
          (cheat_limit + 1 - i)
          ~f:(fun j ->
            match (i, j) with
            | 0, 0 -> [ (0, 0) ]
            | 0, j -> [ (0, j); (0, -j) ]
            | i, 0 -> [ (i, 0); (-i, 0) ]
            | i, j -> [ (i, j); (-i, j); (i, -j); (-i, -j) ]))
    |> List.concat |> List.concat
  in
  let attempt_cheat pos =
    possible_movements
    |> List.filter_map ~f:(fun v ->
           let i, j = v +: pos in
           if v_in_grid grid (i, j) && grid.(i).(j) <> wall then
             Some (l_inf_norm v, grid.(i).(j))
           else None)
  in
  List.fold track ~init:0 ~f:(fun acc (i, j) ->
      let current_time = grid.(i).(j) in
      acc
      + (attempt_cheat (i, j)
        |> List.count ~f:(fun (cheat_dist, n) ->
               n - current_time - cheat_dist >= 100)))

let () = print_solutions (solve 2) (solve 20)
