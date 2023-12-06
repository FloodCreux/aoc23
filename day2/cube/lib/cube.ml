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

(* Part 1 Logic *)
let is_valid_score color score =
  match color with
  | "red" -> int_of_string score <= 12
  | "blue" -> int_of_string score <= 14
  | "green" -> int_of_string score <= 13
  | _ -> false

let parse_round round =
  let split_round = String.split_on_char ',' round in
  List.fold_left
    (fun acc score_str ->
      let color, score = parse_score score_str in
      if is_valid_score color score then
        StringMap.update color
          (function
            | Some existing_score ->
                Some
                  (string_of_int
                     (int_of_string existing_score + int_of_string score))
            | None -> Some score)
          acc
      else StringMap.update color (fun _ -> Some "9999999") acc)
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
        let red_score = int_of_string (StringMap.find "red" game_scores) in
        let blue_score = int_of_string (StringMap.find "blue" game_scores) in
        let green_score = int_of_string (StringMap.find "green" game_scores) in
        if red_score <= 999999 && blue_score <= 99999 && green_score <= 99999
        then solve rest (game_id :: lst)
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

(* Part 2 Logic *)
let parse_round_2 round =
  let split_round = String.split_on_char ',' round in
  List.fold_left
    (fun acc score_str ->
      let color, score = parse_score score_str in
      StringMap.update color
        (function
          | Some existing_score ->
              if int_of_string existing_score >= int_of_string score then
                Some existing_score
              else Some score
          | None -> Some score)
        acc)
    StringMap.empty split_round

let parse_game_rounds_2 rounds =
  let split_rounds = String.split_on_char ';' rounds in
  List.fold_left
    (fun acc round ->
      StringMap.union
        (fun _ a b ->
          if int_of_string a >= int_of_string b then Some a else Some b)
        acc (parse_round_2 round))
    StringMap.empty split_rounds

let parse_game_line_2 line =
  let split_line = String.split_on_char ':' line in
  let game_id =
    int_of_string (List.nth (String.split_on_char ' ' (List.hd split_line)) 1)
  in
  let minimum_numbers = parse_game_rounds_2 (List.nth split_line 1) in
  (game_id, minimum_numbers)

let solve_power_scores games =
  let rec solve games lst =
    match games with
    | [] -> lst
    | (_, game_scores) :: rest ->
        let red_score = int_of_string (StringMap.find "red" game_scores) in
        let blue_score = int_of_string (StringMap.find "blue" game_scores) in
        let green_score = int_of_string (StringMap.find "green" game_scores) in
        let pow = red_score * blue_score * green_score in
        solve rest (pow :: lst)
  in
  solve games []

let solve_part_2 stdin =
  let all_lines = input_lines stdin in
  let games = List.map parse_game_line_2 all_lines in
  let solved_games = solve_power_scores games in
  let rec process_games arr =
    match arr with [] -> 0 | x :: xs -> x + process_games xs
  in
  process_games solved_games
