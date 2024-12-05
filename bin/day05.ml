open Core
open Advent_of_code_2024.Utils

let read_input filename =
  let lines = In_channel.create filename |> In_channel.input_lines in
  let map_rules rules =
    let h = Hashtbl.create (module Int) in
    List.iter rules ~f:(fun (before, after) ->
        Hashtbl.update h before ~f:(function
          | None -> Set.singleton (module Int) after
          | Some s -> Set.add s after));
    h
  in
  let raw_rules, raw_updates = List.split_while ~f:(( <> ) "") lines in
  let rules =
    raw_rules
    |> List.map ~f:(fun s -> Scanf.sscanf s "%d|%d" (Tuple.T2.curry Fn.id))
    |> map_rules
  in
  let updates =
    raw_updates |> List.tl |> Option.value ~default:[]
    |> List.map
         ~f:(List.map ~f:int_of_string << String.split_on_chars ~on:[ ',' ])
  in
  (rules, updates)

let correct_order rules update =
  List.fold_until update
    ~init:(Set.empty (module Int))
    ~f:(fun seen_pages page ->
      if
        Hashtbl.find rules page
        |> Option.map ~f:(Set.are_disjoint seen_pages)
        |> Option.value ~default:true
      then Continue (Set.add seen_pages page)
      else Stop false)
    ~finish:(Fn.const true)

let part_1_ans =
  let rules, updates = read_input "inputs/05-01" in
  updates
  |> List.filter ~f:(correct_order rules)
  |> List.sum
       (module Int)
       ~f:(fun lst -> List.nth_exn lst (List.length lst / 2))

let part_2_ans =
  let rules, updates = read_input "inputs/05-02" in
  let find_bad_pairs update =
    Array.fold_until
      ~init:(Set.empty (module Int))
      ~f:(fun seen_pages page ->
        let inter =
          Hashtbl.find rules page |> Option.map ~f:(Set.inter seen_pages)
        in
        if inter |> Option.map ~f:Set.is_empty |> Option.value ~default:true
        then Continue (Set.add seen_pages page)
        else Stop (Some (inter |> Option.value_exn |> Set.choose_exn, page)))
      ~finish:(Fn.const None) update
  in
  let rec swap_pairs_until_fixed update =
    match find_bad_pairs update with
    | None -> update
    | Some (x, y) ->
        let x_index = Array.findi_exn ~f:(fun _ v -> v = x) update |> fst in
        let y_index = Array.findi_exn ~f:(fun _ v -> v = y) update |> fst in
        update.(x_index) <- y;
        update.(y_index) <- x;
        swap_pairs_until_fixed update
  in
  updates
  |> List.filter ~f:(not << correct_order rules)
  |> List.map ~f:Array.of_list
  |> List.map ~f:swap_pairs_until_fixed
  |> List.sum (module Int) ~f:(fun arr -> arr.(Array.length arr / 2))

let () = print_solutions part_1_ans part_2_ans
