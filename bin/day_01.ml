let patterns =
  [
    ("0", 0);
    ("1", 1);
    ("2", 2);
    ("3", 3);
    ("4", 4);
    ("5", 5);
    ("6", 6);
    ("7", 7);
    ("8", 8);
    ("9", 9);
    ("zero", 0);
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let rec get_first_digit line =
  match line with
  | "" -> raise (Invalid_argument line)
  | line -> (
      let pattern =
        patterns
        |> List.find_opt (fun (pattern, _) ->
               String.starts_with ~prefix:pattern line)
      in
      match pattern with
      | Some (_, value) -> value
      | None -> get_first_digit (String.sub line 1 (String.length line - 1)))

let rec get_last_digit line =
  match line with
  | "" -> raise (Invalid_argument line)
  | line -> (
      let pattern =
        patterns
        |> List.find_opt (fun (pattern, _) ->
               String.ends_with ~suffix:pattern line)
      in
      match pattern with
      | Some (_, value) -> value
      | None -> get_last_digit (String.sub line 0 (String.length line - 1)))

let get_calibration_value line =
  let first_digit = get_first_digit line in
  let second_digit = get_last_digit line in
  (10 * first_digit) + second_digit

let solve () =
  Advent_of_code.Utils.read_lines "data/day_01.txt"
  |> List.map get_calibration_value
  |> List.fold_left ( + ) 0 |> print_int;
  print_newline ()
