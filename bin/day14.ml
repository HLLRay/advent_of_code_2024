open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors

let width = 101
let height = 103

type robot = { p : int * int; v : int * int }

let read_input filename =
  read_repeat_scanf filename "p=%d,%d v=%d,%d\n" (fun px py vx vy ->
      { p = (px, py); v = (vx, vy) })

let simulate_robot t { p; v } =
  { v; p = modVec (p +: (t @: v)) (width, height) }

let part_1_ans : int =
  let score_robots robots =
    let quadrant (x, y) =
      if x = width / 2 || y = height / 2 then None
      else
        Some ((2 * Bool.to_int (x < width / 2)) + Bool.to_int (y < height / 2))
    in
    let quadrant_counts = [| 0; 0; 0; 0 |] in
    List.iter robots ~f:(fun { p; _ } ->
        match quadrant p with
        | Some index -> quadrant_counts.(index) <- quadrant_counts.(index) + 1
        | None -> ());
    Array.fold quadrant_counts ~init:1 ~f:( * )
  in
  read_input "inputs/14-01" |> List.map ~f:(simulate_robot 100) |> score_robots

let grid_from_robots robots =
  let grid = Array.init height ~f:(fun _ -> Array.init width ~f:(const '.')) in
  List.iter robots ~f:(fun { p = px, py; _ } -> grid.(py).(px) <- 'X');
  grid

let num_robots_in_max_col grid =
  List.init width ~f:(fun j -> Array.count grid ~f:(fun row -> row.(j) = 'X'))
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let part_2_ans =
  let robots = ref (read_input "inputs/14-02") in
  let max_robots_in_one_col = ref 0 in
  let best_guess_grid = ref [||] in
  let best_guess_t = ref 0 in
  for t = 1 to 10000 do
    robots := List.map ~f:(simulate_robot 1) !robots;
    let grid = grid_from_robots !robots in
    let current_robots_in_one_col = num_robots_in_max_col grid in
    if current_robots_in_one_col > !max_robots_in_one_col then (
      max_robots_in_one_col := current_robots_in_one_col;
      best_guess_grid := grid;
      best_guess_t := t)
    else ()
  done;
  print_grid !best_guess_grid;
  !best_guess_t

let () = print_solutions part_1_ans part_2_ans
