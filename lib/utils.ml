open Core

let ( << ) = Fn.compose
let ( >> ) x y = Fn.compose y x
let zip_unequal xs ys = List.zip_with_remainder xs ys |> fst
let ( = ) = Stdlib.( = )
let ( <> ) = Stdlib.( <> )

(** [x ^* y] is x to the power of y. Requires: [y] non-negative *)
let ( ^* ) (b : int) (e : int) : int =
  let rec exp_acc (b : int) (e : int) (acc : int) : int =
    match e with
    | 0 -> acc
    | e when e mod 2 = 0 -> exp_acc (b * b) (e / 2) acc
    | e -> exp_acc (b * b) (e / 2) (acc * b)
  in
  if b = 0 && e > 0 then 0 else exp_acc b e 1

let rec gcd (a : int) (b : int) : int = if b = 0 then a else gcd b (a % b)
let gcd_abs (a : int) (b : int) : int = gcd (Int.abs a) (Int.abs b)
let rec num_digits = function 0 -> 0 | n -> 1 + num_digits (n / 10)

let rec f_until ~f ~until args =
  if until args then args else f_until ~f ~until (f args)

let read_lines_fmt filename fmt f =
  let channel = In_channel.create filename in
  let lines = In_channel.input_lines channel in
  List.map ~f:(fun s -> Scanf.sscanf s fmt f) lines

let read_lines_list ?(sep = [ ' ' ]) filename convert =
  let channel = In_channel.create filename in
  let lines = In_channel.input_lines channel in
  List.map ~f:(List.map ~f:convert << String.split_on_chars ~on:sep) lines

let read_grid filename : char array array =
  let channel = In_channel.create filename in
  let lines = In_channel.input_lines channel in
  Array.of_list_map ~f:String.to_array lines

let read_string filename = In_channel.(create filename |> input_all)

let pprint_list pp fmt lst =
  let open Format in
  let sep ff () =
    pp_print_custom_break ~fits:(";", 1, "") ~breaks:(";", 0, "") ff
  in
  printf "[@[<hov 2>";
  pp_print_list ~pp_sep:sep pp fmt lst;
  printf "@]]"

let print_grid (grid : char array array) =
  grid
  |> Array.map ~f:(String.of_char_list << List.of_array)
  |> List.of_array |> String.concat_lines |> print_string

let print_solutions part_1_ans part_2_ans =
  printf "Part 1: %d\nPart 2: %d" part_1_ans part_2_ans
