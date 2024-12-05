open Core
open Advent_of_code_2024.Utils

let part_1_ans =
  let word = "XMAS" |> String.to_list in
  let grid = read_grid "inputs/04-01" in
  let rec search ~word i j (di, dj) =
    match word with
    | [] -> true
    | char :: rest -> (
        try char = grid.(i).(j) && search ~word:rest (i + di) (j + dj) (di, dj)
        with Invalid_argument _ -> false)
  in
  let directions =
    List.cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
    |> List.filter ~f:(( <> ) (0, 0))
  in
  let search_all_dirs ~word i j = List.count ~f:(search ~word i j) directions in
  Array.foldi ~init:0
    ~f:(fun i acc arr ->
      Array.foldi ~init:acc
        ~f:(fun j acc _ -> search_all_dirs ~word i j + acc)
        arr)
    grid

let part_2_ans =
  let grid = read_grid "inputs/04-02" in
  let search i j =
    grid.(i).(j) = 'A'
    &&
    try
      ((grid.(i - 1).(j - 1) = 'M' && grid.(i + 1).(j + 1) = 'S')
      || (grid.(i - 1).(j - 1) = 'S' && grid.(i + 1).(j + 1) = 'M'))
      && ((grid.(i + 1).(j - 1) = 'M' && grid.(i - 1).(j + 1) = 'S')
         || (grid.(i + 1).(j - 1) = 'S' && grid.(i - 1).(j + 1) = 'M'))
    with Invalid_argument _ -> false
  in
  Array.foldi ~init:0
    ~f:(fun i acc arr ->
      Array.foldi ~init:acc
        ~f:(fun j acc _ -> (if search i j then 1 else 0) + acc)
        arr)
    grid

let () = print_solutions part_1_ans part_2_ans
