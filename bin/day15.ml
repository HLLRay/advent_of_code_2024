open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors

let read_input filename =
  let channel = In_channel.create filename in
  let lines = In_channel.input_lines channel in
  let grid_lines, rest =
    List.split_n lines (List.hd lines |> Option.value_exn |> String.length)
  in
  let grid = Array.of_list_map ~f:String.to_array grid_lines in
  let instructions =
    List.tl rest |> Option.value_exn |> String.concat |> String.strip
    |> String.to_array
  in
  (grid, instructions)

let rec attempt_move grid (pos_i, pos_j) move : char array array * (int * int) =
  let v =
    match move with
    | '>' -> (0, 1)
    | 'v' -> (1, 0)
    | '<' -> (0, -1)
    | '^' -> (-1, 0)
    | c -> raise (Failure ("Unknowon direction " ^ String.make 1 c))
  in
  let new_i, new_j = (pos_i, pos_j) +: v in
  let move_success grid =
    grid.(new_i).(new_j) <- grid.(pos_i).(pos_j);
    grid.(pos_i).(pos_j) <- '.';
    (grid, (new_i, new_j))
  in
  let simple_move () =
    if attempt_move grid (new_i, new_j) move |> snd = (new_i, new_j) then
      (grid, (pos_i, pos_j))
    else move_success grid
  in
  let copy_grid () = Array.map grid ~f:Array.copy in
  match grid.(new_i).(new_j) with
  | '.' -> move_success grid
  | '#' -> (grid, (pos_i, pos_j))
  | 'O' -> simple_move ()
  | '[' when fst v = 0 || grid.(new_i).(new_j + 1) <> ']' -> simple_move ()
  | ']' when fst v = 0 || grid.(new_i).(new_j - 1) <> '[' -> simple_move ()
  | '[' ->
      let grid_copy = copy_grid () in
      let grid1, (res1_i, res1_j) = attempt_move grid (new_i, new_j) move in
      if (res1_i, res1_j) <> (new_i, new_j) then
        let grid2, (res2_i, res2_j) =
          attempt_move grid1 (new_i, new_j + 1) move
        in
        if (res2_i, res2_j) <> (new_i, new_j + 1) then move_success grid2
        else (grid_copy, (pos_i, pos_j))
      else (grid_copy, (pos_i, pos_j))
  | ']' ->
      let grid_copy = copy_grid () in
      let grid1, (res1_i, res1_j) = attempt_move grid (new_i, new_j) move in
      if (res1_i, res1_j) <> (new_i, new_j) then
        let grid2, (res2_i, res2_j) =
          attempt_move grid1 (new_i, new_j - 1) move
        in
        if (res2_i, res2_j) <> (new_i, new_j - 1) then move_success grid2
        else (grid_copy, (pos_i, pos_j))
      else (grid_copy, (pos_i, pos_j))
  | c -> raise (Failure ("Unknowon character " ^ String.make 1 c))

let sum_box_coordinates =
  Array.foldi ~init:0 ~f:(fun i acc ->
      Array.foldi ~init:acc ~f:(fun j acc c ->
          if c = 'O' || c = '[' then acc + (100 * i) + j else acc))

let find_robot = find_last_f ~f:(( = ) '@')

let part_1_ans =
  let grid, instructions = read_input "inputs/15-01" in
  Array.fold instructions
    ~init:(grid, find_robot grid)
    ~f:(fun (grid, robot_pos) move -> attempt_move grid robot_pos move)
  |> fst |> sum_box_coordinates

let part_2_ans =
  let grid, instructions = read_input "inputs/15-02" in
  let expanded_grid =
    Array.map grid ~f:(fun row ->
        Array.init
          (Array.length row * 2)
          ~f:(fun j ->
            match row.(j / 2) with
            | '@' when j mod 2 = 1 -> '.'
            | 'O' -> if j mod 2 = 0 then '[' else ']'
            | c -> c))
  in
  Array.fold instructions
    ~init:(expanded_grid, find_robot expanded_grid)
    ~f:(fun (grid, robot_pos) move -> attempt_move grid robot_pos move)
  |> fst |> sum_box_coordinates

let () = print_solutions part_1_ans part_2_ans
