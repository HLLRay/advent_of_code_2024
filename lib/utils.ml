open Core

let read_lines_fmt filename fmt f =
  let channel = In_channel.create filename in
  let lines = In_channel.input_lines channel in
  List.map ~f:(fun s -> Scanf.sscanf s fmt f) lines

let pprint_list pp lst =
  let open Format in
  let sep ff () =
    pp_print_custom_break ~fits:(";", 1, "") ~breaks:(";", 0, "") ff
  in
  printf "[@[<hov 2>";
  pp_print_list ~pp_sep:sep pp std_formatter lst;
  printf "@]]"

let print_solutions part_1_ans part_2_ans =
  printf "Part 1: %d\nPart 2: %d" part_1_ans part_2_ans
