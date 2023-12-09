let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

let calculate_score line =
  let game_split = String.split_on_char ':' line in
  let _game = List.hd game_split in
  let scratcher_split = String.split_on_char '|' (List.nth game_split 1) in
  let scratchers =
    String.split_on_char ' ' (String.trim (List.nth scratcher_split 1))
  in
  let winners =
    String.split_on_char ' ' (String.trim (List.hd scratcher_split))
  in
  let rec get_winners options actual count =
    match options with
    | [] -> actual
    | x :: xs ->
        let winner = try int_of_string x with Failure _ -> -1 in
        if winner > 0 && List.mem (string_of_int winner) winners then
          let score = if actual = 0 then 1 else actual * 2 in
          get_winners xs score (count + 1)
        else get_winners xs actual count
  in
  get_winners scratchers 0 1

let solve_part_1 lst =
  let all_lines = input_lines lst in
  let rec solve lines result =
    match lines with
    | [] -> result
    | x :: xs ->
        let winners = calculate_score x in
        solve xs result + winners
  in
  solve all_lines 0
