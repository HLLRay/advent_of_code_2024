open Core
open Advent_of_code_2024.Utils

let read_input filename =
  read_lines_fmt filename "%[a-z]-%[a-z]" (Tuple.T2.curry Fn.id)

let build_graph edges =
  let neighbours_map = Hashtbl.create (module String) in
  let vertices =
    List.fold edges
      ~init:(Set.empty (module String))
      ~f:(fun vertices (u, v) ->
        let add = Fn.flip Set.add in
        let add_neighbour x neighbours =
          neighbours
          |> Option.value ~default:(Set.empty (module String))
          |> add x
        in
        Hashtbl.update neighbours_map u ~f:(add_neighbour v);
        Hashtbl.update neighbours_map v ~f:(add_neighbour u);
        vertices |> add u |> add v)
  in
  (vertices, Hashtbl.find_exn neighbours_map)

let part_1_ans =
  let has_t s = String.get s 0 = 't' in
  let vertices, neighbours = read_input "inputs/23-01" |> build_graph in
  Set.fold vertices ~init:(0, vertices)
    ~f:(fun (total_triangles, unchecked_verts) v ->
      let unchecked_neighbours =
        Set.filter (neighbours v) ~f:(Set.mem unchecked_verts) |> Set.to_list
      in
      let new_triangles =
        pairs unchecked_neighbours
        |> List.filter ~f:(fun (u, w) -> Set.mem (neighbours u) w)
        |> List.filter ~f:(fun (u, w) -> has_t v || has_t u || has_t w)
        |> List.length
      in
      (new_triangles + total_triangles, Set.remove unchecked_verts v))
  |> fst

let part_2_ans =
  let vertices, neighbours_map = read_input "inputs/23-02" |> build_graph in
  let are_neighbours u v = Set.mem (neighbours_map u) v in
  (* Assume all u in neighbours are adjacent to v *)
  let rec find_max_clique v neighbours =
    neighbours |> Set.to_list |> pairs
    |> List.find ~f:(fun (u, w) -> not (are_neighbours u w))
    |> function
    | None -> Set.add neighbours v
    | Some (u, w) ->
        let c1 = find_max_clique v (Set.remove neighbours u) in
        let c2 = find_max_clique v (Set.remove neighbours w) in
        if Set.length c1 > Set.length c2 then c1 else c2
  in
  let max_clique =
    vertices |> Set.to_list
    |> List.map ~f:(fun v -> find_max_clique v (neighbours_map v))
    |> List.max_elt ~compare:(fun c1 c2 ->
           Int.compare (Set.length c1) (Set.length c2))
  in
  max_clique |> Option.value_exn |> Set.to_list
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","

let () = printf "Part 1: %d\nPart 2: %s\n" part_1_ans part_2_ans
