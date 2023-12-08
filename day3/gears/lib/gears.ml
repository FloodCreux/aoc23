let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

let get_digits cur =
  let rec process_curr i digits =
    if i >= String.length cur then digits
    else
      let find_next_digit s =
        let len = String.length s in
        let rec find_start k =
          if k >= len then None
          else if is_digit s.[k] then Some k
          else find_start (k + 1)
        in
        let rec find_end j =
          if j >= len || not (is_digit s.[j]) then j - 1 else find_end (j + 1)
        in
        match find_start i with
        | Some start_pos ->
            let end_pos = find_end start_pos in
            Some (start_pos, end_pos)
        | None -> None
      in
      match find_next_digit cur with
      | Some (start_pos, end_pos) ->
          let digit = String.sub cur start_pos (end_pos - start_pos + 1) in
          process_curr (end_pos + 1) ((digit, start_pos, end_pos) :: digits)
      | None -> List.rev digits
  in
  process_curr 0 []

let filter_digits cur line prev next =
  let check_ends line start stop =
    let right =
      if start > 0 then String.get line (start - 1) != '.' else false
    in
    let left =
      if stop < String.length line - 1 then String.get line (stop + 1) != '.'
      else false
    in
    right || left
  in
  let check_all line start stop =
    let start = if start = 0 then 0 else start - 1 in
    let stop = if stop = String.length line - 1 then stop else stop + 1 in
    let rec check_middle i =
      if i > stop then false
      else
        let c = String.get line i in
        if c != '.' then true else check_middle (i + 1)
    in
    check_middle start
  in
  List.filter
    (fun (_d, start, stop) ->
      match (prev, next) with
      | None, None ->
          let result = check_ends line start stop in
          result
      | Some prev, None ->
          let result =
            check_ends line start stop || check_all prev start stop
          in
          result
      | None, Some next ->
          let result =
            check_ends line start stop || check_all next start stop
          in
          result
      | Some prev, Some next ->
          let result =
            check_ends line start stop || check_all prev start stop
            || check_all next start stop
          in
          result)
    cur

let solve_part_1 lst =
  let all_lines = input_lines lst in
  let rec solve prev lst result =
    match lst with
    | [] -> result
    | curr :: rest ->
        let next =
          if List.length rest >= 1 then Some (List.hd rest) else None
        in
        let digits = get_digits curr in
        solve (Some curr) rest (result @ filter_digits digits curr prev next)
  in
  let results = solve None all_lines [] in
  List.fold_left (fun acc (d, _, _) -> acc + int_of_string d) 0 results

let get_asterisks cur =
  let rec process i positions =
    if i >= String.length cur then positions
    else if cur.[i] = '*' then process (i + 1) (i :: positions)
    else process (i + 1) positions
  in
  process 0 []

let filter_digits_2 positions prev next result =
  let is_digit line pos =
    let c = String.get line pos in
    if c >= '0' && c <= '9' then true else false
  in
  let get_digit line i =
    let pos = List.nth positions i in
    let rec get_left j res =
      if j < 0 then res
      else if is_digit line j then
        let c = String.get line j in
        if c >= '0' && c <= '9' then
          get_left (j - 1) (int_of_string (String.make 1 c ^ string_of_int res))
        else res
      else res
    in
    let rec get_right j res =
      if j < 0 then res
      else if is_digit line j then
        let c = String.get line j in
        if c >= '0' && c <= '9' then
          get_right (j - 1)
            (int_of_string (string_of_int res ^ String.make 1 c))
        else res
      else res
    in
    let left = get_left pos 0 in
    let right = get_right pos 0 in
    left * right
  in
  match (prev, next) with
  | None, None -> result
  | Some prev, None ->
      let rec check i =
        if i >= List.length positions then result
        else
          let prev_dig = get_digit prev i in
          print_int prev_dig;
          print_newline ();
          prev_dig + check (i + 1)
      in
      check result
  | None, Some next ->
      let rec check i =
        if i >= List.length positions then result
        else
          let next_dig = get_digit next i in
          next_dig + check (i + 1)
      in
      check result
  | Some prev, Some next ->
      let rec check i =
        if i >= List.length positions then result
        else
          let next_dig = get_digit next i in
          let prev_dig = get_digit prev i in
          next_dig + prev_dig + check (i + 1)
      in
      check result

let solve_part_2 lst =
  let all_lines = input_lines lst in
  let rec solve prev lst result =
    match lst with
    | [] -> result
    | curr :: rest ->
        let next =
          if List.length rest >= 1 then Some (List.hd rest) else None
        in
        let digits = get_asterisks curr in
        solve (Some curr) rest (filter_digits_2 digits prev next 0 :: result)
  in
  let lst = solve None all_lines [] in
  List.fold_left (fun acc x -> acc + x) 0 lst
