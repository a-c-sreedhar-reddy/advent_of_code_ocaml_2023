open Angstrom

let pair a b = (a, b)

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let color = string "blue" <|> string "green" <|> string "red"
let game_number = string "Game " *> integer <* char ':'
let cubes = lift2 pair (char ' ' *> integer <* char ' ') color
let turn = sep_by (char ',') cubes
let turns = sep_by1 (char ';') turn
let parse_game = lift2 pair game_number turns
let max_red = 12
let max_green = 13
let max_blue = 14

let get_cubes_from_turn turn ~c =
  turn
  |> List.find_map (fun (value, color) ->
         if c = color then Some value else None)
  |> Option.value ~default:0

let is_game_possible (_, turns) =
  let valid_red =
    turns
    |> List.map (get_cubes_from_turn ~c:"red")
    |> List.for_all (( >= ) max_red)
  in

  let valid_blue =
    turns
    |> List.map (get_cubes_from_turn ~c:"blue")
    |> List.for_all (( >= ) max_blue)
  in

  let valid_green =
    turns
    |> List.map (get_cubes_from_turn ~c:"green")
    |> List.for_all (( >= ) max_green)
  in

  valid_red && valid_blue && valid_green

let game_power (_, turns) =
  let max_red =
    turns |> List.map (get_cubes_from_turn ~c:"red") |> List.fold_left max 0
  in

  let max_blue =
    turns |> List.map (get_cubes_from_turn ~c:"blue") |> List.fold_left max 0
  in
  let max_green =
    turns |> List.map (get_cubes_from_turn ~c:"green") |> List.fold_left max 0
  in

  max_blue * max_red * max_green

let solve () =
  Advent_of_code.Utils.read_lines "data/day_02.txt"
  |> List.map (parse_string ~consume:All parse_game)
  |> List.map Result.get_ok
  |> List.filter is_game_possible
  |> List.map (fun (id, _) -> id)
  |> List.fold_left ( + ) 0 |> print_int |> print_newline;

  Advent_of_code.Utils.read_lines "data/day_02.txt"
  |> List.map (parse_string ~consume:All parse_game)
  |> List.map Result.get_ok |> List.map game_power |> List.fold_left ( + ) 0
  |> print_int |> print_newline
