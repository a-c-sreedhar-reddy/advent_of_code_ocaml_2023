let read_lines file_name =
  let ic = open_in file_name in
  let rec read_all_lines ic acc =
    try
      let line = input_line ic in
      read_all_lines ic (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_all_lines ic []

let sum arr = arr |> List.fold_left ( + ) 0

let rev_string str =
  str |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq
  |> String.of_seq
