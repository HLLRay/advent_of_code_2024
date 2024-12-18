open Core
open Advent_of_code_2024.Utils

type internal_state = { a : int; b : int; c : int; pc : int; out : int list }
type state = Running of internal_state | Halted of int list

let read_input filename =
  let channel = Scanf.Scanning.open_in filename in
  let init_state =
    Scanf.bscanf channel "Register A: %d\nRegister B: %d\nRegister C: %d "
      (fun a b c -> Running { a; b; c; pc = 0; out = [] })
  in
  let program =
    Scanf.bscanf channel "Program: %s" (fun s ->
        s
        |> String.split_on_chars ~on:[ ',' ]
        |> List.map ~f:Int.of_string |> List.to_array)
  in
  (init_state, program)

let is_halted = function Halted _ -> true | Running _ -> false

let extract_output = function
  | Halted out -> List.rev out
  | Running { out; _ } -> List.rev out

let step program = function
  | Running state when state.pc + 1 < Array.length program -> (
      let { a; b; c; pc; out } = state in
      let opcode = program.(pc) in
      let literal_operand = program.(pc + 1) in
      let combo_operand =
        match literal_operand with
        | 0 | 1 | 2 | 3 -> literal_operand
        | 4 -> a
        | 5 -> b
        | 6 -> c
        | 7 -> raise (Invalid_argument "7 is reserved")
        | _ -> raise (Invalid_argument "3-bit numbers only")
      in
      match opcode with
      | 0 -> Running { state with a = a / (2 ^* combo_operand); pc = pc + 2 }
      | 1 -> Running { state with b = b lxor literal_operand; pc = pc + 2 }
      | 2 -> Running { state with b = combo_operand mod 8; pc = pc + 2 }
      | 3 ->
          Running
            { state with pc = (if a = 0 then pc + 2 else literal_operand) }
      | 4 -> Running { state with b = b lxor c; pc = pc + 2 }
      | 5 ->
          Running { state with out = (combo_operand mod 8) :: out; pc = pc + 2 }
      | 6 -> Running { state with b = a / (2 ^* combo_operand); pc = pc + 2 }
      | 7 -> Running { state with c = a / (2 ^* combo_operand); pc = pc + 2 }
      | _ -> raise (Invalid_argument "3-bit numberes only"))
  | Running { out; _ } -> Halted out
  | Halted _ -> raise (Invalid_argument "Machine already halted")

let part_1_ans =
  let state, program = read_input "inputs/17-01" in
  f_until ~f:(step program) ~until:is_halted state
  |> extract_output |> List.map ~f:Int.to_string |> String.concat ~sep:","

let solve program =
  let rec all_solutions acc bs solutions =
    match bs with
    | [] -> acc :: solutions
    | b :: bs ->
        let branches =
          List.init 8 ~f:Fn.id
          |> List.filter ~f:(fun a ->
                 b = a lxor 4 lxor (((8 * acc) + a) / (2 ^* (a lxor 1))) mod 8)
        in
        List.fold branches ~init:solutions ~f:(fun solutions a ->
            all_solutions ((8 * acc) + a) bs solutions)
  in
  let inverted_program_list = program |> List.of_array |> List.rev in
  List.min_elt ~compare:Int.compare (all_solutions 0 inverted_program_list [])
  |> Option.value_exn

let part_2_ans =
  let _, program = read_input "inputs/17-02" in
  let res = solve program in
  assert (
    f_until ~f:(step program) ~until:is_halted
      (Running { a = res; b = 0; c = 0; pc = 0; out = [] })
    |> extract_output = List.of_array program);
  res

let () = printf "Part 1: %s\nPart 2: %d" part_1_ans part_2_ans
