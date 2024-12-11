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
