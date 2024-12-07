open Core
open Advent_of_code_2024.Utils

let guard_pos_from_grid grid =
  Array.find_mapi grid ~f:(fun i row ->
      Array.find_mapi row ~f:(fun j cell ->
          if cell = '^' || cell = '>' || cell = 'v' || cell = '<' then
            Some (i, j)
          else None))

let guard_v_from_grid grid (i, j) =
  match grid.(i).(j) with
  | '^' -> (-1, 0)
  | '>' -> (0, 1)
  | 'v' -> (1, 0)
  | '<' -> (0, -1)
  | _ -> raise (Invalid_argument "Unknown character")

let turn_right (i, j) = (j, -i)

let guard_in_map grid (i, j) =
  let max_i = Array.length grid in
  let max_j = if max_i > 0 then Array.length grid.(0) else 0 in
  0 <= i && i < max_i && 0 <= j && j < max_j

let has_obstacle grid (i, j) = guard_in_map grid (i, j) && grid.(i).(j) = '#'

let step grid guard_pos guard_v =
  let new_forward_pos = ref (!guard_pos ++ !guard_v) in
  while has_obstacle grid !new_forward_pos do
    guard_v := turn_right !guard_v;
    new_forward_pos := !guard_pos ++ !guard_v
  done;
  !new_forward_pos

let part_1_ans =
  let grid = read_grid "inputs/06-01" in
  let guard_pos = ref (guard_pos_from_grid grid |> Option.value_exn) in
  let guard_v = ref (guard_v_from_grid grid !guard_pos) in
  let num_visited = ref 0 in
  while guard_in_map grid !guard_pos do
    let i, j = !guard_pos in
    num_visited := !num_visited + if grid.(i).(j) <> 'X' then 1 else 0;
    grid.(i).(j) <- 'X';
    guard_pos := step grid guard_pos guard_v
  done;
  !num_visited

let part_2_ans =
  let grid = read_grid "inputs/06-02" in
  let guard_pos = ref (guard_pos_from_grid grid |> Option.value_exn) in
  let guard_v = ref (guard_v_from_grid grid !guard_pos) in
  let num_solutions = ref 0 in
  let seen_vs_at_pos = Array.map ~f:(Array.map ~f:(const [])) grid in
  let v_in v = List.exists ~f:(( = ) v) in
  let add_v_if_not_seen seen_vs_at_pos (i, j) v =
    let seen = seen_vs_at_pos.(i).(j) in
    if v_in v seen then () else seen_vs_at_pos.(i).(j) <- v :: seen
  in
  let try_obstacle_at_pos (i, j) =
    grid.(i).(j) <- '#';
    let seen_vs_at_pos =
      Array.map ~f:(fun row -> Array.map ~f:Fn.id row) seen_vs_at_pos
    in
    let guard_pos = ref !guard_pos in
    let guard_v = ref !guard_v in
    guard_pos := step grid guard_pos guard_v;
    while
      guard_in_map grid !guard_pos
      && not (v_in !guard_v seen_vs_at_pos.(fst !guard_pos).(snd !guard_pos))
    do
      add_v_if_not_seen seen_vs_at_pos !guard_pos !guard_v;
      guard_pos := step grid guard_pos guard_v
    done;
    grid.(i).(j) <- '.';
    guard_in_map grid !guard_pos
  in
  while guard_in_map grid !guard_pos do
    add_v_if_not_seen seen_vs_at_pos !guard_pos !guard_v;
    let new_pos = step grid guard_pos guard_v in
    num_solutions :=
      !num_solutions
      +
      if
        guard_in_map grid new_pos
        && seen_vs_at_pos.(fst new_pos).(snd new_pos) = []
        && try_obstacle_at_pos new_pos
      then 1
      else 0;
    guard_pos := new_pos
  done;
  !num_solutions

let () = print_solutions part_1_ans part_2_ans
