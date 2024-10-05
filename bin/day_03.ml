let get_numbers line ~line_number =
  let regex = Str.regexp "[0-9]+" in
  let rec aux pos acc =
    try
      let _ = Str.search_forward regex line pos in
      let matched_str = Str.matched_string line in
      let start_pos = Str.match_beginning () in
      let end_pos = Str.match_end () in
      aux end_pos ((matched_str, line_number, start_pos) :: acc)
    with Not_found -> List.rev acc
  in
  aux 0 []

let get_adjacent_chars line_number start_pos end_pos lines =
  let line = List.nth lines line_number in
  let line_len = String.length line in
  let aux_start, aux_end =
    (max 0 (start_pos - 1), min (line_len - 1) (end_pos + 1))
  in
  let prev_line =
    if line_number = 0 then None else List.nth_opt lines (line_number - 1)
  in
  let prev_line_chars =
    prev_line
    |> Option.map (fun s -> String.sub s aux_start (aux_end - aux_start + 1))
    |> Option.map (fun s -> s |> String.to_seq |> List.of_seq)
    |> Option.value ~default:[]
  in

  let next_line = List.nth_opt lines (line_number + 1) in
  let next_line_chars =
    next_line
    |> Option.map (fun s -> String.sub s aux_start (aux_end - aux_start + 1))
    |> Option.map (fun s -> s |> String.to_seq |> List.of_seq)
    |> Option.value ~default:[]
  in

  let prev_char =
    match start_pos with
    | 0 -> []
    | start_pos -> [ String.get line (start_pos - 1) ]
  in

  let next_char =
    if end_pos = line_len - 1 then [] else [ String.get line (end_pos + 1) ]
  in

  prev_line_chars @ next_line_chars @ prev_char @ next_char

let gears_in_line line_number line =
  line |> String.to_seq |> List.of_seq
  |> List.mapi (fun i ch -> (ch, line_number, i))
  |> List.filter (fun (ch, _, _) -> ch = '*')
  |> List.map (fun (_, r, c) -> (r, c))

let is_gear_adjacent_to_part_number gear part_number lines =
  let str, line_number, start_pos = part_number in
  let end_pos = start_pos + String.length str - 1 in
  let line_len = lines |> List.hd |> String.length in
  let aux_start, aux_end =
    (max 0 (start_pos - 1), min (line_len - 1) (end_pos + 1))
  in
  let prev_line =
    if line_number = 0 then []
    else
      List.init
        (aux_end - aux_start + 1)
        (fun i -> (line_number - 1, aux_start + i))
  in

  let next_line =
    if line_number = List.length lines - 1 then []
    else
      List.init
        (aux_end - aux_start + 1)
        (fun i -> (line_number + 1, aux_start + i))
  in

  let prev_char =
    match start_pos with
    | 0 -> []
    | start_pos -> [ (line_number, start_pos - 1) ]
  in

  let next_char =
    if end_pos = line_len - 1 then [] else [ (line_number, end_pos + 1) ]
  in

  prev_line @ next_line @ prev_char @ next_char
  |> List.exists (fun (x, y) ->
         let gear_x, gear_y = gear in
         x = gear_x && y = gear_y)

let solve () =
  let lines = Advent_of_code.Utils.read_lines "data/day_03.txt" in
  let part_numbers =
    lines
    |> List.mapi (fun line_number line -> get_numbers ~line_number line)
    |> List.flatten
    |> List.filter (fun (str, line_number, start_pos) ->
           let adjacent_chars =
             get_adjacent_chars line_number start_pos
               (start_pos + String.length str - 1)
               lines
           in
           adjacent_chars
           |> List.exists (fun char ->
                  match char with
                  | '0' .. '9' -> false
                  | '.' -> false
                  | _ -> true))
  in

  part_numbers
  |> List.map (fun (str, _, _) -> int_of_string str)
  |> List.fold_left ( + ) 0 |> print_int |> print_newline;

  lines |> List.mapi gears_in_line |> List.flatten
  |> List.map (fun gear ->
         ( gear,
           part_numbers
           |> List.filter (fun part_number ->
                  is_gear_adjacent_to_part_number gear part_number lines) ))
  |> List.filter_map (fun (gear, adj) ->
         match adj with
         | [ (number1, _, _); (number2, _, _) ] ->
             Some (gear, int_of_string number1, int_of_string number2)
         | _ -> None)
  |> List.map (fun (_, a, b) -> a * b)
  |> List.fold_left ( + ) 0 |> print_int |> print_newline
