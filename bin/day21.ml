open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors

let read_input filename =
  let channel = In_channel.create filename in
  let lines = In_channel.input_lines channel in
  List.map lines ~f:String.to_list

let base_dist_tbl = const 1
let confirm_key = 'A'
let directional_keys = [ '^'; 'A'; '<'; 'v'; '>' ]
let numeric_keys = [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0'; 'A' ]

let directional_key_coords = function
  | '^' -> (0, 1)
  | 'A' -> (0, 2)
  | '<' -> (1, 0)
  | 'v' -> (1, 1)
  | '>' -> (1, 2)
  | key -> raise (Invalid_argument ("Unknown key " ^ String.of_char key))

let numeric_key_coords = function
  | '7' -> (0, 0)
  | '8' -> (0, 1)
  | '9' -> (0, 2)
  | '4' -> (1, 0)
  | '5' -> (1, 1)
  | '6' -> (1, 2)
  | '1' -> (2, 0)
  | '2' -> (2, 1)
  | '3' -> (2, 2)
  | '0' -> (3, 1)
  | 'A' -> (3, 2)
  | key -> raise (Invalid_argument ("Unknown key " ^ String.of_char key))

let directional_coords_illegal = ( = ) (0, 0)
let numeric_coords_illegal = ( = ) (3, 0)

let cost_of_path base_tbl path =
  List.fold path ~init:(confirm_key, 0) ~f:(fun (prev_key, acc) key ->
      (key, acc + base_tbl (prev_key, key)))
  |> snd

let gen_tbl keys key_coords illegal_coords base_tbl =
  (* Please paths can't be too long *)
  let gen_paths src_coords dst_coords =
    let di, dj = dst_coords -: src_coords in
    let hori_movements =
      List.init (Int.abs dj) ~f:(const (if dj > 0 then (0, 1) else (0, -1)))
    in
    let vert_movements =
      List.init (Int.abs di) ~f:(const (if di > 0 then (1, 0) else (-1, 0)))
    in
    gen_permutations (hori_movements @ vert_movements)
    |> List.filter_map ~f:(fun path ->
           let partial_sums =
             List.folding_map path ~init:src_coords ~f:(fun partial_sum v ->
                 (partial_sum +: v, partial_sum +: v))
           in
           let convert_to_symbol_path =
             List.map ~f:(function
               | 0, -1 -> '<'
               | 0, 1 -> '>'
               | -1, 0 -> '^'
               | 1, 0 -> 'v'
               | _ -> raise (Invalid_argument "Unknown vector"))
           in
           if List.exists partial_sums ~f:illegal_coords then None
           else Some (convert_to_symbol_path path @ [ confirm_key ]))
  in
  let h = Hashtbl.create (module CharPair) in
  List.cartesian_product keys keys
  |> List.iter ~f:(fun (src, dst) ->
         if src = dst then Hashtbl.set h ~key:(src, dst) ~data:1
         else
           let src_coords = key_coords src in
           let dst_coords = key_coords dst in
           let paths = gen_paths src_coords dst_coords in
           let cost =
             List.map ~f:(cost_of_path base_tbl) paths
             |> List.min_elt ~compare:Int.compare
             |> Option.value_exn
           in
           Hashtbl.set h ~key:(src, dst) ~data:cost);
  Hashtbl.find_exn h

let get_sequence_numeric_value sequence =
  sequence |> List.filter ~f:Char.is_digit |> String.of_list |> Int.of_string

let solve_with_num_directional_robots num_directional_robots =
  let sequences = read_input "inputs/21-01" in
  let intermediate_tbl =
    Fn.apply_n_times ~n:num_directional_robots
      (gen_tbl directional_keys directional_key_coords
         directional_coords_illegal)
      base_dist_tbl
  in
  let final_tbl =
    gen_tbl numeric_keys numeric_key_coords numeric_coords_illegal
      intermediate_tbl
  in
  List.sum
    (module Int)
    sequences
    ~f:(fun sequence ->
      let cost = cost_of_path final_tbl sequence in
      let sequence_numeric_value = get_sequence_numeric_value sequence in
      cost * sequence_numeric_value)

let part_1_ans = solve_with_num_directional_robots 2
let part_2_ans = solve_with_num_directional_robots 25
let () = print_solutions part_1_ans part_2_ans
