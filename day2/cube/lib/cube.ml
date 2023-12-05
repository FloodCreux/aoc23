let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

module StringMap = Map.Make (String)

let parse_score score =
  let split_score = String.split_on_char ' ' (String.trim score) in
  (List.nth split_score 1, List.hd split_score)

let parse_round round =
  let split_round = String.split_on_char ',' round in
  List.fold_left
    (fun acc score_str ->
      let color, score = parse_score score_str in
      StringMap.update color
        (function
          | Some existing_score ->
              Some
                (string_of_int
                   (int_of_string existing_score + int_of_string score))
          | None -> Some score)
        acc)
    StringMap.empty split_round

let parse_game_rounds rounds =
  let split_rounds = String.split_on_char ';' rounds in
  List.fold_left
    (fun acc round ->
      StringMap.union
        (fun _ a b -> Some (string_of_int (int_of_string a + int_of_string b)))
        acc (parse_round round))
    StringMap.empty split_rounds

let parse_game_line line =
  let split_line = String.split_on_char ':' line in
  let game_id =
    int_of_string (List.nth (String.split_on_char ' ' (List.hd split_line)) 1)
  in
  let combined_scores = parse_game_rounds (List.nth split_line 1) in
  (game_id, combined_scores)

let solve_scores games =
  let rec solve games lst =
    match games with
    | [] -> lst
    | (game_id, game_scores) :: rest ->
        let red_score = StringMap.find "red" game_scores in
        let blue_score = StringMap.find "blue" game_scores in
        let green_score = StringMap.find "green" game_scores in
        Printf.printf "Game %d: red (%s), blue (%s), green (%s)\n" game_id
          red_score blue_score green_score;
        if
          int_of_string red_score <= 12
          && int_of_string blue_score <= 14
          && int_of_string green_score <= 13
        then (
          Printf.printf "Game %d works\n" game_id;
          solve rest (game_id :: lst))
        else solve rest lst
  in
  solve games []

let solve_part_1 stdin =
  let all_lines = input_lines stdin in
  let games = List.map parse_game_line all_lines in
  let solved_games = solve_scores games in
  let rec process_games arr =
    match arr with [] -> 0 | x :: xs -> x + process_games xs
  in
  process_games solved_games
