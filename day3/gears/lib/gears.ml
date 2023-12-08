let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

let check_digits cur prev next =
  let rec process_curr i digits =
    if i >= String.length cur then digits
    else
      let find_next_digit s =
        let len = String.length s in
        let rec find_start i =
          if i >= len then None
          else if is_digit s.[i] then Some i
          else find_start (i + 1)
        in
        let rec find_end i =
          if i >= len || not (is_digit s.[i]) then i - 1 else find_end (i + 1)
        in
        match find_start i with
        | Some start_pos ->
            let end_pos = find_end start_pos in
            Some (start_pos, end_pos)
        | None -> None
      in
      let curr_digits = find_next_digit cur in
      match prev with Some prev -> curr_digits | None -> curr_digits
  in
  match next with Some next -> curr_digits | None -> curr_digits

let solve_part_1 lst =
  let all_lines = input_lines lst in
  let solve arr =
    let len = Array.length arr in
    let rec process i =
      if i >= len then ()
      else
        let current = arr.(i) in
        let prev = if i > 0 then Some arr.(i - 1) else None in
        let next = if i < len - 1 then Some arr.(i + 1) else None in
        check_digits current prev next
    in
    process 0
  in
  solve (Array.of_list all_lines)
