open Core
open Utils

let dfs ~(neighbours : 'a -> 'a list) ~(seen : 'a -> bool)
    ~(mark_seen : 'a -> unit) (v : 'a) : 'a Sequence.t =
  let open Sequence in
  unfold_step ~init:[ v ] ~f:(function
    | [] -> Step.Done
    | v :: rest ->
        if seen v then Step.Skip { state = rest }
        else (
          mark_seen v;
          Step.Yield
            {
              value = v;
              state = (v |> neighbours |> List.filter ~f:(not << seen)) @ rest;
            }))

module MinHeap = struct
  type 'a t = {
    mutable data : 'a array;
    mutable size : int;
    compare : 'a -> 'a -> int;
  }

  let swap heap i j =
    let temp = heap.data.(i) in
    heap.data.(i) <- heap.data.(j);
    heap.data.(j) <- temp

  let rec heapify_down heap i =
    let l = (2 * i) + 1 in
    let r = (2 * i) + 2 in
    let smaller =
      if l < heap.size && heap.compare heap.data.(l) heap.data.(i) < 0 then l
      else if r < heap.size && heap.compare heap.data.(r) heap.data.(i) < 0 then
        r
      else i
    in
    if smaller <> i then (
      swap heap i smaller;
      heapify_down heap smaller)
    else ()

  let rec heapify_up heap i =
    if i > 0 then
      let parent = (i - 1) / 2 in
      if heap.compare heap.data.(i) heap.data.(parent) < 0 then (
        swap heap i parent;
        heapify_up heap parent)
      else ()

  let add heap x =
    if heap.size = Array.length heap.data then
      heap.data <-
        Array.append heap.data
          (Array.init (Int.max 1 (Array.length heap.data)) ~f:(const x))
    else heap.data.(heap.size) <- x;
    heap.size <- heap.size + 1;
    heapify_up heap (heap.size - 1)

  let pop heap =
    if heap.size = 0 then None
    else
      let root = heap.data.(0) in
      heap.size <- heap.size - 1;
      heap.data.(0) <- heap.data.(heap.size);
      heapify_down heap 0;
      Some root

  let peak heap = if heap.size = 0 then None else Some heap.data.(0)
  let length heap = heap.size

  let create ~(compare : 'a -> 'a -> int) (lst : 'a list) =
    let heap = { data = [||]; size = 0; compare } in
    List.iter lst ~f:(add heap);
    heap
end

let dijkstra ~(neighbours : 'a -> (int * 'a) list)
    ~(distance : 'a -> int option) ~(set_distance : 'a -> int -> unit)
    ~(set_visited : 'a -> unit) ~(set_pred : bool -> 'a -> 'a -> unit)
    ~(stop : 'a -> bool) (start : 'a) : unit =
  set_distance start 0;
  let heap =
    MinHeap.create
      ~compare:(fun (d1, _) (d2, _) -> Int.compare d1 d2)
      [ (0, start) ]
  in
  let res = ref None in
  while MinHeap.length heap > 0 && Option.is_none !res do
    let d, v = Option.value_exn (MinHeap.pop heap) in
    if
      Option.map (distance v) ~f:(fun x -> x >= d)
      |> Option.value ~default:false
    then
      v |> neighbours
      |> List.iter ~f:(fun (w, u) ->
             let new_d = d + w in
             if
               Option.map (distance u) ~f:(fun x -> x > new_d)
               |> Option.value ~default:true
             then (
               set_distance u new_d;
               set_pred true u v;
               MinHeap.add heap (new_d, u))
             else if
               Option.map (distance u) ~f:(fun x -> x = new_d)
               |> Option.value ~default:false
             then set_pred false u v
             else ())
    else ();
    set_visited v;
    if stop v then res := Some (d, v) else ()
  done
