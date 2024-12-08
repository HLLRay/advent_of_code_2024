open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors

let create_antennas_by_freq grid : (char, (int * int) list) Hashtbl.t =
  let antennas_by_freq = Hashtbl.create (module Char) in
  Array.iteri
    ~f:(fun i row ->
      Array.iteri row ~f:(fun j cell ->
          if Char.is_alphanum cell then
            Hashtbl.update antennas_by_freq cell ~f:(fun locs ->
                (i, j) :: Option.value locs ~default:[])
          else ()))
    grid;
  antennas_by_freq

let iterate_through_freqs antennas_by_freq ~f =
  Hashtbl.iter antennas_by_freq ~f:(fun locs ->
      let seq = Sequence.of_list locs in
      Sequence.cartesian_product seq seq
      |> Sequence.filter ~f:(fun (v1, v2) -> v1 <> v2)
      |> Sequence.iter ~f)

let part_1_ans =
  let grid = read_grid "inputs/08-01" in
  let num_antinodes = ref 0 in
  let antennas_by_freq = create_antennas_by_freq grid in
  let add_antinode_one_dir v1 v2 =
    let antinode = (2 @: v1) -: v2 in
    let i, j = antinode in
    if v_in_grid grid antinode && grid.(i).(j) <> '#' then (
      grid.(i).(j) <- '#';
      num_antinodes := !num_antinodes + 1)
    else ()
  in
  iterate_through_freqs antennas_by_freq ~f:(fun (v1, v2) ->
      add_antinode_one_dir v1 v2;
      add_antinode_one_dir v2 v1);
  !num_antinodes

let part_2_ans =
  let grid = read_grid "inputs/08-02" in
  let num_antinodes = ref 0 in
  let antennas_by_freq = create_antennas_by_freq grid in
  let add_antinodes v1 v2 =
    let dir_unminimised = v1 -: v2 in
    let dir = (Tuple.T2.uncurry gcd_abs) dir_unminimised /: dir_unminimised in
    let add_antinode (i, j) =
      if v_in_grid grid (i, j) && grid.(i).(j) <> '#' then (
        grid.(i).(j) <- '#';
        num_antinodes := !num_antinodes + 1)
      else ()
    in
    ignore
      (f_until
         ~f:(fun current_antinode ->
           add_antinode current_antinode;
           current_antinode +: dir)
         ~until:(not << v_in_grid grid)
         v1);
    ignore
      (f_until
         ~f:(fun current_antinode ->
           add_antinode current_antinode;
           current_antinode -: dir)
         ~until:(not << v_in_grid grid)
         v1)
  in
  iterate_through_freqs antennas_by_freq ~f:(Tuple.T2.uncurry add_antinodes);
  !num_antinodes

let () = print_solutions part_1_ans part_2_ans
