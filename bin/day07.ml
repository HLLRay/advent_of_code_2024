open Core
open Advent_of_code_2024.Utils

type equation = { result : int; operands : int list }

let parse filname =
  read_lines_list filname ~sep:[ ':' ] Fn.id
  |> List.map ~f:(function
       | [ result; raw_operands ] ->
           let operands =
             raw_operands |> String.strip
             |> String.split_on_chars ~on:[ ' ' ]
             |> List.map ~f:int_of_string
           in
           { result = int_of_string result; operands }
       | _ -> raise (Failure "Failed to parse line"))

let rec can_be_solved operators { result; operands } =
  match operands with
  | [] -> raise (Invalid_argument "Empty operands")
  | [ res ] -> res = result
  | x1 :: x2 :: rest ->
      List.exists operators ~f:(fun op ->
          can_be_solved operators { result; operands = op x1 x2 :: rest })

let part_1_ans =
  let operators = [ ( + ); ( * ) ] in
  let equations = parse "inputs/07-01" in
  equations
  |> List.filter ~f:(can_be_solved operators)
  |> List.sum (module Int) ~f:(fun { result; _ } -> result)

let part_2_ans =
  let ( || ) x y = (x * (10 ^* num_digits y)) + y in
  let operators = [ ( + ); ( * ); ( || ) ] in
  let equations = parse "inputs/07-02" in
  equations
  |> List.filter ~f:(can_be_solved operators)
  |> List.sum (module Int) ~f:(fun { result; _ } -> result)

let () = print_solutions part_1_ans part_2_ans
