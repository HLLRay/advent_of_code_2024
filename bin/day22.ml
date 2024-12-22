open Core
open Advent_of_code_2024.Utils
open Advent_of_code_2024.Vectors

let read_input filename = read_lines_fmt filename "%d" Fn.id

let evolve secret =
  let mix_and_prune f secret =
    let n = f secret in
    secret lxor n mod 16777216
  in
  secret
  |> mix_and_prune (( * ) 64)
  |> mix_and_prune (fun s -> s / 32)
  |> mix_and_prune (( * ) 2048)

let part_1_ans =
  let secrets = read_input "inputs/22-01" in
  secrets |> List.sum (module Int) ~f:(Fn.apply_n_times ~n:2000 evolve)

let part_2_ans =
  let init_4d_grid value =
    init_grid 19 19 (fun _ _ -> init_grid 19 19 (const (const value)))
  in
  let secrets = read_input "inputs/22-02" in
  let returns = init_4d_grid 0 in
  let max_price = ref 0 in
  List.iter secrets ~f:(fun secret ->
      let seen = init_4d_grid false in
      let current_secret = ref secret in
      let current_diffs = ref [] in
      for _ = 1 to 2000 do
        let new_secret = evolve !current_secret in
        let price = new_secret mod 10 in
        let diff = price - (!current_secret mod 10) in
        if List.length !current_diffs < 4 then
          current_diffs := diff :: !current_diffs
        else (
          current_diffs := diff :: List.take !current_diffs 3;
          let i, j, k, l =
            match !current_diffs with
            | [ i; j; k; l ] -> (i + 9, j + 9, k + 9, l + 9)
            | _ -> raise (Failure "Diffs should not contain more than 4 items")
          in
          if not seen.(i).(j).(k).(l) then (
            seen.(i).(j).(k).(l) <- true;
            let new_price = returns.(i).(j).(k).(l) + price in
            returns.(i).(j).(k).(l) <- new_price;
            if new_price > !max_price then max_price := new_price else ())
          else ());
        current_secret := new_secret
      done);
  !max_price

let () = print_solutions part_1_ans part_2_ans
