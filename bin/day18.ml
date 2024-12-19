open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Graphs
open Advent_of_code_2024.Vectors

let grid_size = 71
let read_input filename = read_lines_fmt filename "%d,%d" (Tuple.T2.curry Fn.id)
let start_pos = (0, 0)
let end_pos = (grid_size - 1, grid_size - 1)

let part_1_ans =
  let bytes = read_input "inputs/18-01" in
  let grid = init_grid grid_size grid_size (const (const '.')) in
  let distances = init_grid grid_size grid_size (const (const None)) in
  let visited = init_grid grid_size grid_size (const (const false)) in
  List.take bytes 1024 |> List.iter ~f:(fun (i, j) -> grid.(i).(j) <- '#');
  dijkstra
    ~neighbours:(fun pos ->
      orth_neighbours_in_grid grid pos
      |> List.filter ~f:(fun (i, j) -> grid.(i).(j) <> '#')
      |> List.map ~f:(fun pos -> (1, pos)))
    ~distance:(fun (i, j) -> distances.(i).(j))
    ~set_distance:(fun (i, j) d -> distances.(i).(j) <- Some d)
    ~set_visited:(fun (i, j) -> visited.(i).(j) <- true)
    ~set_pred:(fun _ _ _ -> ())
    ~stop:(fun _ ->
      orth_neighbours_in_grid grid end_pos
      |> List.for_all ~f:(fun (i, j) -> visited.(i).(j)))
    start_pos
  |> ignore;
  distances.(fst end_pos).(snd end_pos) |> Option.value_exn

let part_2_ans =
  let bytes = read_input "inputs/18-02" in
  let grid = init_grid grid_size grid_size (const (const '.')) in
  let end_reachable () =
    let seen = init_grid grid_size grid_size (const (const false)) in
    dfs
      ~neighbours:(fun pos ->
        orth_neighbours_in_grid grid pos
        |> List.filter ~f:(fun (i, j) -> grid.(i).(j) <> '#'))
      ~seen:(fun (i, j) -> seen.(i).(j))
      ~mark_seen:(fun (i, j) -> seen.(i).(j) <- true)
      start_pos
    |> Sequence.find ~f:(( = ) end_pos)
    |> Option.is_some
  in
  List.find bytes ~f:(fun (i, j) ->
      grid.(i).(j) <- '#';
      end_reachable () |> not)
  |> Option.value_exn

let () =
  printf "Part 1: %d\nPart 2: %d,%d\n " part_1_ans (fst part_2_ans)
    (snd part_2_ans)
