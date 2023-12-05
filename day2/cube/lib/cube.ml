let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

let parse_game_line line =
  let split_line = String.split_on_char ':' line in
  let game = List.hd split_line in
  let game_id = int_of_string (List.nth (String.split_on_char ' ' game) 1) in
  let game_scores = String.split_on_char ';' (List.nth split_line 1) in
  Printf.printf "Game: %d " game_id;
  List.iter (fun x -> Printf.printf "%s | " (String.trim x)) game_scores;
  print_newline ()

let solve_part_1 stdin =
  let all_lines = input_lines stdin in
  List.iter parse_game_line all_lines
