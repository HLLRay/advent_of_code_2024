open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors
open Advent_of_code_2024.Graphs

type node = { pos : int * int; dir : int * int }

let neighbours grid { pos; dir } =
  let new_i, new_j = pos +: dir in
  (if v_in_grid grid (new_i, new_j) && grid.(new_i).(new_j) <> '#' then
     [ (1, { pos = (new_i, new_j); dir }) ]
   else [])
  @ [
      (1000, { pos; dir = turn_right dir }); (1000, { pos; dir = turn_left dir });
    ]

let grid = read_grid "inputs/16-01"
let end_pos = find_last_f ~f:(( = ) 'E') grid
let start_pos = find_last_f ~f:(( = ) 'S') grid

let distances =
  grid |> Array.map ~f:(Array.map ~f:(fun _ -> Hashtbl.create (module IntPair)))

let preds =
  grid |> Array.map ~f:(Array.map ~f:(fun _ -> Hashtbl.create (module IntPair)))

let end_pos_distances = distances.(fst end_pos).(snd end_pos)

let () =
  dijkstra
    ~distance:(fun { pos = i, j; dir; _ } -> Hashtbl.find distances.(i).(j) dir)
    ~set_distance:(fun { pos = i, j; dir; _ } d ->
      Hashtbl.set distances.(i).(j) ~key:dir ~data:d)
    ~neighbours:(neighbours grid)
    ~set_pred:(fun overwrite { pos = i, j; dir } u ->
      let h = preds.(i).(j) in
      if overwrite then Hashtbl.set h ~key:dir ~data:[ u ]
      else
        Hashtbl.update h dir ~f:(function None -> [ u ] | Some ps -> u :: ps))
    ~set_visited:(const ()) ~stop:(const false)
    { pos = start_pos; dir = (0, 1) }

let part_1_ans =
  Int.min
    (Hashtbl.find_exn end_pos_distances (0, 1))
    (Hashtbl.find_exn end_pos_distances (-1, 0))

let rec mark_on_path node =
  let i, j = node.pos in
  if node.pos = end_pos then
    printf "%d, %d, %d, %d\n" i j (fst node.dir) (snd node.dir)
  else ();
  grid.(i).(j) <- 'O';
  Hashtbl.find preds.(i).(j) node.dir
  |> Option.value ~default:[] |> List.iter ~f:mark_on_path

let part_2_ans =
  if Hashtbl.find_exn end_pos_distances (0, 1) <= part_1_ans then
    mark_on_path { pos = end_pos; dir = (0, 1) }
  else ();
  if Hashtbl.find_exn end_pos_distances (-1, 0) <= part_1_ans then
    mark_on_path { pos = end_pos; dir = (-1, 0) }
  else ();
  Array.fold grid ~init:0 ~f:(fun acc ->
      Array.fold ~init:acc ~f:(fun acc char ->
          if char = 'O' then acc + 1 else acc))

let () = print_solutions part_1_ans part_2_ans
