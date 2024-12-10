open Core
open Advent_of_code_2024.Utils

let decompress_disk_map =
  List.fold ~init:([], false, 0, 0, [])
    ~f:(fun (map, free_space, id, next_index, space_map) num_blocks ->
      let next_map =
        Fn.apply_n_times ~n:num_blocks
          (fun xs -> (if free_space then -1 else id) :: xs)
          map
      in
      let next_free_space = not free_space in
      let next_id = if free_space then id else id + 1 in
      let next_next_index = next_index + num_blocks in
      let next_space_map =
        if free_space then (next_index, num_blocks) :: space_map else space_map
      in
      (next_map, next_free_space, next_id, next_next_index, next_space_map))
  >> fun (map, _, _, _, space_map) ->
  ( map |> List.drop_while ~f:(( = ) (-1)) |> List.rev |> Array.of_list,
    space_map |> List.rev |> Array.of_list )

let checksum =
  Array.foldi ~init:0 ~f:(fun i acc id ->
      if id = -1 then acc else acc + (id * i))

let part_1_ans =
  let disk_map_compressed =
    read_string "inputs/09-01" |> String.strip |> String.to_list
    |> List.map ~f:Char.get_digit_exn
  in
  let disk_map, _ = decompress_disk_map disk_map_compressed in
  let next_free_space i =
    f_until ~f:(( + ) 1) ~until:(fun j -> disk_map.(j) = -1) i
  in
  let prev_file_block i =
    f_until ~f:(fun j -> j - 1) ~until:(fun j -> disk_map.(j) <> -1) i
  in
  let first_free_space_i = ref (next_free_space 0) in
  let last_file_block_i = ref (prev_file_block (Array.length disk_map - 1)) in
  while !first_free_space_i < !last_file_block_i do
    disk_map.(!first_free_space_i) <- disk_map.(!last_file_block_i);
    disk_map.(!last_file_block_i) <- -1;
    first_free_space_i := next_free_space !first_free_space_i;
    last_file_block_i := prev_file_block !last_file_block_i
  done;
  checksum disk_map

exception Not_found_s

let part_2_ans =
  let disk_map_compressed =
    read_string "inputs/09-02" |> String.strip |> String.to_list
    |> List.map ~f:Char.get_digit_exn
  in
  let disk_map, space_map = decompress_disk_map disk_map_compressed in
  let move_file i j =
    let file_id = disk_map.(i) in
    let num_moved =
      f_until
        ~f:(fun offset ->
          disk_map.(j + offset) <- file_id;
          disk_map.(i + offset) <- -1;
          offset + 1)
        ~until:(fun offset ->
          i + offset >= Array.length disk_map
          || disk_map.(i + offset) <> file_id)
        0
    in
    let space_map_i, (space_i, capacity) =
      Array.findi_exn space_map ~f:(fun _ (space_i, _) -> space_i = j)
    in
    space_map.(space_map_i) <- (space_i + num_moved, capacity - num_moved)
  in
  let prev_untried_file_index currunt_i prev_file_id =
    let prev_file_end =
      f_until
        ~f:(fun j -> j - 1)
        ~until:(fun j -> j = 0 || disk_map.(j) = prev_file_id)
        currunt_i
    in
    let prev_file_start =
      f_until
        ~f:(fun j -> j - 1)
        ~until:(fun j -> j = 0 || disk_map.(j - 1) <> prev_file_id)
        prev_file_end
    in
    (prev_file_start, prev_file_end - prev_file_start + 1)
  in
  let find_first_free_space_i (current_index, size) =
    try
      Array.find space_map ~f:(fun (i, capacity) ->
          if i > current_index then raise Not_found_s else capacity >= size)
      |> Option.map ~f:fst
    with Not_found_s -> None
  in
  let disk_size = Array.length disk_map in
  let last_untried_file =
    ref (prev_untried_file_index (disk_size - 1) disk_map.(disk_size - 1))
  in
  while fst !last_untried_file > 0 do
    let current_file_id = disk_map.(fst !last_untried_file) in
    (match find_first_free_space_i !last_untried_file with
    | None -> ()
    | Some first_free_space ->
        move_file (fst !last_untried_file) first_free_space);
    last_untried_file :=
      prev_untried_file_index (fst !last_untried_file) (current_file_id - 1)
  done;
  checksum disk_map

let () = print_solutions part_1_ans part_2_ans
