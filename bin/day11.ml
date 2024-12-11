open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors

let read_input filename =
  In_channel.create filename |> In_channel.input_all |> String.strip
  |> String.split_on_chars ~on:[ ' ' ]
  |> List.map ~f:Int.of_string

let num_stones_cache = Hashtbl.create (module IntPair)

let rec num_stones num gen =
  Hashtbl.update_and_return num_stones_cache (num, gen) ~f:(function
    | Some res -> res
    | None ->
        if gen = 0 then 1
        else if num = 0 then num_stones 1 (gen - 1)
        else
          let digits = num_digits num in
          if digits mod 2 = 0 then
            let upper = num / (10 ^* (digits / 2)) in
            let lower = num mod (10 ^* (digits / 2)) in
            num_stones upper (gen - 1) + num_stones lower (gen - 1)
          else num_stones (num * 2024) (gen - 1))

let part_1_ans =
  read_input "inputs/11-01"
  |> List.sum (module Int) ~f:(fun num -> num_stones num 25)

let part_2_ans =
  read_input "inputs/11-02"
  |> List.sum (module Int) ~f:(fun num -> num_stones num 75)

let () = print_solutions part_1_ans part_2_ans
