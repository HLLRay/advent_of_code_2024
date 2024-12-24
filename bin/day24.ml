open Core
open Advent_of_code_2024.Utils

let ( let* ) v f = Lazy.bind v ~f

let hashmap_lazy_find h k =
  let* v = lazy (Hashtbl.find_exn h k) in
  v

let read_input filename =
  let channel = Scanf.Scanning.open_in filename in
  let rec scan_wires wires =
    try
      scan_wires
        (Scanf.bscanf channel "%[0-9a-z]: %d\n" (fun wire value ->
             (wire, value) :: wires))
    with
    | Scanf.Scan_failure _ -> wires
    | exn -> raise exn
  in
  let rec scan_gates gates =
    try
      scan_gates
        (Scanf.bscanf channel " %s %s %s -> %s\n" (fun a b gate c ->
             (a, b, gate, c) :: gates))
    with
    | End_of_file -> gates
    | exn -> raise exn
  in
  let wires = scan_wires [] in
  let gates = scan_gates [] in
  (wires, gates)

let build_wires input_wires gates =
  let wires_fn wires wire value =
    Hashtbl.set wires ~key:wire ~data:(Lazy.return value)
  in

  let gates_fn wires a gate b c =
    let op =
      match gate with
      | "OR" -> ( lor )
      | "AND" -> ( land )
      | "XOR" -> ( lxor )
      | _ -> raise (Invalid_argument ("Unknown gate: " ^ gate))
    in
    Hashtbl.set wires ~key:c
      ~data:
        (let* a = hashmap_lazy_find wires a in
         let* b = hashmap_lazy_find wires b in
         lazy (op a b))
  in

  let wires = Hashtbl.create (module String) in
  List.iter input_wires ~f:(fun (wire, value) -> wires_fn wires wire value);
  List.iter gates ~f:(fun (a, b, gate, c) -> gates_fn wires a b gate c);
  wires

let part_1_ans =
  let input_wires, gates = read_input "inputs/24-01" in
  let wires = build_wires input_wires gates in
  Hashtbl.keys wires
  |> List.filter ~f:(fun k -> String.get k 0 = 'z')
  |> List.sort ~compare:String.compare
  |> List.rev
  |> List.map ~f:(fun k -> Lazy.force (Hashtbl.find_exn wires k))
  |> List.fold ~init:0 ~f:(fun acc b -> (2 * acc) + b)

let part_2_ans =
  let input_wires, gates = read_input "inputs/24-02" in
  let num_inputs = List.length input_wires / 2 in
  let swaps =
    [ ("vss", "z14"); ("kdh", "hjf"); ("kpp", "z31"); ("z35", "sgj") ]
  in
  let swap wires (k1, k2) =
    let temp1 = Hashtbl.find_exn wires k1 in
    let temp2 = Hashtbl.find_exn wires k2 in
    Hashtbl.set wires ~key:k1 ~data:temp2;
    Hashtbl.set wires ~key:k2 ~data:temp1
  in
  let build_wires_with_swaps input_wires gates =
    let wires = build_wires input_wires gates in
    List.iter swaps ~f:(swap wires);
    wires
  in
  for i = 0 to num_inputs - 1 do
    let input1 =
      List.init (2 * num_inputs) ~f:(fun j ->
          if j < num_inputs then
            if j <> i then (Printf.sprintf "x%02d" j, 0)
            else (Printf.sprintf "x%02d" j, 1)
          else
            let j = j mod num_inputs in
            if j <> i then (Printf.sprintf "y%02d" j, 0)
            else (Printf.sprintf "y%02d" j, 0))
    in
    let wires = build_wires_with_swaps input1 gates in
    let output =
      Hashtbl.find_exn wires (Printf.sprintf "z%02d" i) |> Lazy.force
    in
    if output <> 1 then
      printf "At posiiton %d, input x = 1, y = 0 yields output z = %d\n" i
        output
    else ();
    let input2 =
      List.init (2 * num_inputs) ~f:(fun j ->
          if j < num_inputs then
            if j <> i then (Printf.sprintf "x%02d" j, 0)
            else (Printf.sprintf "x%02d" j, 0)
          else
            let j = j mod num_inputs in
            if j <> i then (Printf.sprintf "y%02d" j, 0)
            else (Printf.sprintf "y%02d" j, 1))
    in
    let wires = build_wires_with_swaps input2 gates in
    let output =
      Hashtbl.find_exn wires (Printf.sprintf "z%02d" i) |> Lazy.force
    in
    if output <> 1 then
      printf "At posiiton %d, input x = 0, y = 1 yields output z = %d\n" i
        output
    else ();
    let input3 =
      List.init (2 * num_inputs) ~f:(fun j ->
          if j < num_inputs then
            if j <> i then (Printf.sprintf "x%02d" j, 0)
            else (Printf.sprintf "x%02d" j, 1)
          else
            let j = j mod num_inputs in
            if j <> i then (Printf.sprintf "y%02d" j, 0)
            else (Printf.sprintf "y%02d" j, 1))
    in
    let wires = build_wires_with_swaps input3 gates in
    let output1 =
      Hashtbl.find_exn wires (Printf.sprintf "z%02d" i) |> Lazy.force
    in
    let output2 =
      Hashtbl.find_exn wires (Printf.sprintf "z%02d" (i + 1)) |> Lazy.force
    in
    if output1 <> 0 || output2 <> 1 then
      printf
        "At posiiton %d, input x = 1, y = 1 yields output z = %d and carry = %d \n"
        i output1 output2
    else ()
  done;
  swaps
  |> List.concat_map ~f:(fun (x, y) -> [ x; y ])
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","

let () = printf "Part 1: %d\nPart 2: %s\n" part_1_ans part_2_ans
