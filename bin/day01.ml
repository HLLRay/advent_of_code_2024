open Core
open Advent_of_code_2024.Utils

let part_1_ans =
  let sort = List.sort ~compare:Int.compare in
  read_lines_fmt "inputs/01-01" "%d %d" (Tuple.T2.curry Fn.id)
  |> List.unzip |> Tuple.T2.map ~f:sort
  |> Tuple.T2.uncurry List.zip_exn
  |> List.sum (module Int) ~f:(fun (a, b) -> Int.abs (a - b))

let part_2_ans =
  let left, right =
    read_lines_fmt "inputs/01-02" "%d %d" (Tuple.T2.curry Fn.id) |> List.unzip
  in
  let h = Hashtbl.create (module Int) in
  List.iter ~f:(Hashtbl.incr h) right;
  List.fold_left ~init:0
    ~f:(fun acc n ->
      n |> Hashtbl.find h |> Option.value ~default:0 |> ( * ) n |> ( + ) acc)
    left

let () = print_solutions part_1_ans part_2_ans
