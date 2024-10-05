open Angstrom

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let white_space = take_while (fun c -> c = ' ')
let card_number = string "Card" *> white_space *> integer <* char ':'
let numbers = white_space *> sep_by white_space integer <* white_space
let card_numbers = both (numbers <* char '|') numbers
let card = both card_number card_numbers
let rec pow a b = match b with 0 -> 1 | b -> a * pow a (b - 1)

let solve () =
  let cards =
    Advent_of_code.Utils.read_lines "data/day_04.txt"
    |> List.map (parse_string ~consume:All card)
    |> List.map Result.get_ok
  in

  cards
  |> List.map (fun (_, (winning_number, my_numbers)) ->
         my_numbers
         |> List.filter (fun n -> List.mem n winning_number)
         |> List.length)
  |> List.filter (fun n -> n <> 0)
  |> List.map (fun n -> pow 2 (n - 1))
  |> List.fold_left ( + ) 0 |> print_int |> print_newline;

  let _, cards_count =
    cards
    |> List.fold_left
         (fun (next_cards_count, cards_count) (_, (winning_number, my_numbers)) ->
           let times = List.hd next_cards_count in
           let next_cards_count = List.tl next_cards_count in

           let matched_numbers =
             my_numbers
             |> List.filter (fun n -> List.mem n winning_number)
             |> List.length
           in

           let next_cards_count =
             next_cards_count
             |> List.mapi (fun i count ->
                    if i < matched_numbers then count + times else count)
           in

           (next_cards_count, cards_count + times))
         (List.init (List.length cards) (fun _ -> 1), 0)
  in
  cards_count |> print_int |> print_newline
