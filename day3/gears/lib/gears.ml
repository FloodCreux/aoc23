let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

let check_digits cur _prev _next =
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

let solve_part_1 lst =
  let all_lines = input_lines lst in
  let rec solve = function
    | [] -> ()
    | curr :: rest ->
        let prev = if rest = [] then None else Some (List.hd rest) in
        let next =
          if List.length rest > 1 then Some (List.nth rest 1) else None
        in
        let digits = check_digits curr prev next in
        List.iter
          (fun (s, start, stop) ->
            Printf.printf "%s: start - %d, end - %d\n" s start stop)
          digits;
        solve rest
  in
  solve all_lines
