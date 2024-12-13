open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors

type machine = { a : int * int; b : int * int; p : int * int }

let read_input filename =
  read_repeat_scanf filename
    "Button A: X+%d, Y+%d Button B: X+%d, Y+%d Prize: X=%d, Y=%d "
    (fun ax ay bx by px py -> { a = (ax, ay); b = (bx, by); p = (px, py) })

let cost (an, bn) = (3 * an) + bn

(* Assumes these situations do not happen *)
(* Could be dealt with separatley but long *)
exception Colinear of int * int * int * int
exception ZeroCoords of int * int * int * int

let optimise_machine { a = ax, ay; b = bx, by; p = px, py } =
  if colinear (ax, ay) (bx, by) then raise (Colinear (ax, ay, bx, by))
  else if ax * ay * bx * by = 0 then raise (ZeroCoords (ax, ay, bx, by))
  else
    (* alpha * a + beta * b = p *)
    let beta_numerator = (px * ay) - (py * ax) in
    (* Not 0 because matrix is full rank *)
    let beta_denominator = (ay * bx) - (ax * by) in
    if beta_numerator mod beta_denominator = 0 then
      let beta = beta_numerator / beta_denominator in
      if beta >= 0 then
        let alpha_numerator = py - (by * beta) in
        let alpha_denominator = ay in
        if alpha_numerator mod alpha_denominator = 0 then
          let alpha = alpha_numerator / alpha_denominator in
          Some (cost (alpha, beta))
        else None
      else None
    else None

let part_1_ans =
  read_input "inputs/13-01"
  |> List.map ~f:optimise_machine
  |> List.sum (module Int) ~f:(Option.value ~default:0)

let part_2_ans =
  let conversion_error = 10000000000000 in
  read_input "inputs/13-02"
  |> List.map ~f:(fun { a; b; p } ->
         { a; b; p = p +: (conversion_error, conversion_error) })
  |> List.map ~f:optimise_machine
  |> List.sum (module Int) ~f:(Option.value ~default:0)

let () = print_solutions part_1_ans part_2_ans
