open Core
open Advent_of_code_2024.Utils

type func = Mul | Do | Dont

type argParser = {
  func : func;
  parsing_arg : int option;
  parsed_args_rev : int list;
}

type reader = ParseFunc of char list | ParseArgs of argParser
type state = { doMul : bool; acc : int }

let digit_limit = 3

let interpret ~enable_conditionals (reader, { doMul; acc }) char =
  let digit_of_char d = d |> Char.get_digit |> Option.value_exn in
  let max_digits_reached n = n > 10 ^* (digit_limit - 1) in
  let extract_func = function
    | 'l' :: 'u' :: 'm' :: _ -> Some Mul
    | 'o' :: 'd' :: _ -> Some Do
    | 't' :: '\'' :: 'n' :: 'o' :: 'd' :: _ -> Some Dont
    | _ -> None
  in
  let applyFunc ~enable_conditionals func args { doMul; acc } =
    match (func, args) with
    | Mul, [ x; y ] ->
        Some { acc = (if doMul then acc + (x * y) else acc); doMul }
    | Do, [] when enable_conditionals -> Some { acc; doMul = true }
    | Dont, [] when enable_conditionals -> Some { acc; doMul = false }
    | _ -> None
  in
  match (reader, char) with
  | ParseFunc ident_rev, char when Char.is_alpha char ->
      (ParseFunc (char :: ident_rev), { doMul; acc })
  | ParseFunc ident_rev, '\'' -> (ParseFunc ('\'' :: ident_rev), { doMul; acc })
  | ParseFunc ident_rev, '(' -> (
      match extract_func ident_rev with
      | Some func ->
          ( ParseArgs { func; parsed_args_rev = []; parsing_arg = None },
            { doMul; acc } )
      | None -> (ParseFunc [], { doMul; acc }))
  | ParseArgs { parsing_arg; parsed_args_rev; func }, char
    when Char.is_digit char
         && not (max_digits_reached (Option.value ~default:0 parsing_arg)) ->
      let new_arg =
        (Option.value ~default:0 parsing_arg * 10) + digit_of_char char
      in
      ( ParseArgs { parsing_arg = Some new_arg; parsed_args_rev; func },
        { doMul; acc } )
  | ( ParseArgs { parsing_arg = Some parsing_arg_value; parsed_args_rev; func },
      ',' ) ->
      ( ParseArgs
          {
            parsing_arg = None;
            parsed_args_rev = parsing_arg_value :: parsed_args_rev;
            func;
          },
        { doMul; acc } )
  | ParseArgs { parsing_arg; parsed_args_rev; func }, ')' ->
      let args =
        match parsing_arg with
        | Some parsing_arg_value ->
            List.rev (parsing_arg_value :: parsed_args_rev)
        | None -> List.rev parsed_args_rev
      in
      let newState =
        Option.value
          (applyFunc ~enable_conditionals func args { doMul; acc })
          ~default:{ doMul; acc }
      in
      (ParseFunc [], newState)
  | _ -> (ParseFunc [], { doMul; acc })

let part_1_ans =
  "inputs/03-01" |> read_string |> String.to_list
  |> List.fold_left
       ~init:(ParseFunc [], { doMul = true; acc = 0 })
       ~f:(interpret ~enable_conditionals:false)
  |> fun (_, { acc; _ }) -> acc

let part_2_ans =
  "inputs/03-02" |> read_string |> String.to_list
  |> List.fold_left
       ~init:(ParseFunc [], { doMul = true; acc = 0 })
       ~f:(interpret ~enable_conditionals:true)
  |> fun (_, { acc; _ }) -> acc

let () = print_solutions part_1_ans part_2_ans
