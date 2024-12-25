open Core
open Advent_of_code_2024.Utils

let read_input filename =
  let channel = Scanf.Scanning.open_in filename in
  let rec loop (keys, locks) =
    let scan_top_row () =
      Scanf.bscanf channel " %c%c%c%c%c\n" (fun a b c d e ->
          match (a, b, c, d, e) with
          | '#', '#', '#', '#', '#' -> `Lock
          | '.', '.', '.', '.', '.' -> `Key
          | _ ->
              raise
                (Invalid_argument "First row must be either all '#' or all '.'"))
    in
    let scan_middle_row current_schematic =
      Scanf.bscanf channel "%c%c%c%c%c\n" (fun a b c d e ->
          List.map2_exn [ a; b; c; d; e ] current_schematic
            ~f:(fun char column -> column + if char = '#' then 1 else 0))
    in
    let scan_bottom_row () =
      Scanf.bscanf channel "%c%c%c%c%c\n" (fun a b c d e ->
          match (a, b, c, d, e) with
          | '#', '#', '#', '#', '#' -> `Key
          | '.', '.', '.', '.', '.' -> `Lock
          | _ ->
              raise
                (Invalid_argument "Last row must be either all '#' or all '.'"))
    in
    try
      let schematic_type = scan_top_row () in
      let schematic = Fn.apply_n_times ~n:5 scan_middle_row [ 0; 0; 0; 0; 0 ] in
      let schematic_type_bottom = scan_bottom_row () in
      assert (schematic_type = schematic_type_bottom);
      let acc =
        if schematic_type = `Key then (schematic :: keys, locks)
        else (keys, schematic :: locks)
      in
      loop acc
    with
    | End_of_file -> (keys, locks)
    | exn -> raise exn
  in
  loop ([], [])

let part_1_ans =
  let keys, locks = read_input "inputs/25-01" in
  let fit (key, lock) =
    List.zip_exn key lock
    |> List.for_all ~f:(fun (key_col, lock_col) -> key_col + lock_col <= 5)
  in
  List.cartesian_product keys locks |> List.count ~f:fit

let () = print_solutions part_1_ans 0
