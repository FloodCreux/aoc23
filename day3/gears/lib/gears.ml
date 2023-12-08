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
  let check_line line start stop =
    let right = if start > 0 then String.get line (start - 1) else '.' in
    let left =
      if stop < String.length line - 1 then String.get line (stop + 1) else '.'
    in
    right != '.' || left != '.'
  in
  List.filter
    (fun (_d, start, stop) ->
      match (prev, next) with
      | None, None -> check_line line start stop
      | Some prev, None ->
          check_line line start stop || check_line prev start stop
      | None, Some next ->
          check_line line start stop || check_line next start stop
      | Some prev, Some next ->
          check_line line start stop || check_line prev start stop
          || check_line next start stop)
    cur

let solve_part_1 lst =
  let all_lines = input_lines lst in
  let rec solve lst result =
    match lst with
    | [] -> result
    | curr :: rest ->
        let prev = if rest = [] then None else Some (List.hd rest) in
        let next =
          if List.length rest > 1 then Some (List.nth rest 1) else None
        in
        let digits = get_digits curr in
        solve rest (filter_digits digits curr prev next)
  in
  solve all_lines []
