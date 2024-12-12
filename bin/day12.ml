open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors
open Advent_of_code_2024.Graphs

let find_region map seen (i, j) =
  let plant_type = map.(i).(j) in
  dfs
    ~neighbours:(fun (i, j) ->
      orth_neighbours_in_grid map (i, j)
      |> List.filter ~f:(fun (new_i, new_j) -> map.(new_i).(new_j) = plant_type))
    ~seen:(fun (i, j) -> seen.(i).(j))
    ~mark_seen:(fun (i, j) -> seen.(i).(j) <- true)
    (i, j)

let calculate_total_cost score_region map =
  let seen = map |> Array.map ~f:(Array.map ~f:(const false)) in
  map
  |> Array.foldi ~init:0 ~f:(fun i acc ->
         Array.foldi ~init:acc ~f:(fun j acc _ ->
             acc
             +
             if not seen.(i).(j) then
               find_region map seen (i, j) |> score_region
             else 0))

let part_1_ans =
  let map = read_grid "inputs/12-01" in
  let score_region region =
    let plots_in_region = Set.of_sequence (module IntPair) region in
    let area = Set.length plots_in_region in
    let perimeter =
      Set.sum
        (module Int)
        ~f:(fun pos ->
          orth_neighbours pos
          |> List.filter ~f:(Set.mem plots_in_region >> not)
          |> List.length)
        plots_in_region
    in
    area * perimeter
  in
  calculate_total_cost score_region map

let part_2_ans =
  let map = read_grid "inputs/12-02" in
  let score_region region =
    let plots_in_region = Set.of_sequence (module IntPair) region in
    let area = Set.length plots_in_region in
    let plots_list = Set.to_list plots_in_region in
    let i_coords = List.map ~f:fst plots_list in
    let j_coords = List.map ~f:snd plots_list in
    let min_i =
      List.min_elt ~compare:Int.compare i_coords |> Option.value_exn
    in
    let min_j =
      List.min_elt ~compare:Int.compare j_coords |> Option.value_exn
    in
    let max_i =
      List.max_elt ~compare:Int.compare i_coords |> Option.value_exn
    in
    let max_j =
      List.max_elt ~compare:Int.compare j_coords |> Option.value_exn
    in
    let count_num_sides_of_row i =
      List.init (max_j - min_j + 2) ~f:(( + ) min_j)
      |> List.fold ~init:(0, 0) ~f:(fun (num_sides, prev_side) j ->
             let new_side =
               (if Set.mem plots_in_region (i, j) then 1 else 0)
               + if Set.mem plots_in_region (i - 1, j) then -1 else 0
             in
             ( (num_sides
               + if prev_side <> new_side && new_side <> 0 then 1 else 0),
               new_side ))
      |> fst
    in
    let count_num_sides_of_col j =
      List.init (max_i - min_i + 2) ~f:(( + ) min_i)
      |> List.fold ~init:(0, 0) ~f:(fun (num_sides, prev_side) i ->
             let new_side =
               (if Set.mem plots_in_region (i, j) then 1 else 0)
               + if Set.mem plots_in_region (i, j - 1) then -1 else 0
             in
             ( (num_sides
               + if prev_side <> new_side && new_side <> 0 then 1 else 0),
               new_side ))
      |> fst
    in
    let sides =
      (List.init (max_i - min_i + 2) ~f:(( + ) min_i)
      |> List.sum (module Int) ~f:count_num_sides_of_row)
      + (List.init (max_j - min_j + 2) ~f:(( + ) min_j)
        |> List.sum (module Int) ~f:count_num_sides_of_col)
    in
    area * sides
  in
  calculate_total_cost score_region map

let () = print_solutions part_1_ans part_2_ans
