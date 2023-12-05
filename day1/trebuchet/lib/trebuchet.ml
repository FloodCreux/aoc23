let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

let line_adjustments line =
  let n = String.length line in
  let rec aux first last i =
    if i >= n then
      match (first, last) with
      | Some f, Some l ->
          Some (int_of_string (String.make 1 f ^ String.make 1 l))
      | _ -> None
    else
      let c = String.get line i in
      if Char.code c >= Char.code '0' && Char.code c <= Char.code '9' then
        match first with
        | None -> aux (Some c) (Some c) (i + 1)
        | Some _ -> aux first (Some c) (i + 1)
      else aux first last (i + 1)
  in
  aux None None 0

let solve_part_1 file_channel =
  let all_lines = input_lines file_channel in
  let rec process_lines_1 arr =
    match arr with
    | [] -> 0
    | x :: xs -> (
        match line_adjustments x with
        | Some x -> x + process_lines_1 xs
        | None -> process_lines_1 xs)
  in
  process_lines_1 all_lines

let get_number_from_string line =
  List.iter
    (fun x -> if String.starts_with line ~prefix:x then return Some (1, x))
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven" ]

let line_adjustments_part_2 line =
  let n = String.length line in
  let rec aux first last i =
    if i >= n then
      match (first, last) with
      | Some f, Some l ->
          Some (int_of_string (String.make 1 f ^ String.make 1 l))
      | _ -> None
    else
      let c = String.get line i in
      if Char.code c >= Char.code '0' && Char.code c <= Char.code '9' then
        match first with
        | None -> aux (Some c) (Some c) (i + 1)
        | Some _ -> aux first (Some c) (i + 1)
      else
        match
          get_number_from_string (String.sub line i (String.length line - i))
        with
        | Some (num, str) -> (
            match first with
            | None -> aux (Some num) (Some num) (i + String.length str)
            | Some _ -> aux first (Some num) (i + String.length str))
        | None -> aux first last (i + 1)
  in
  aux None None 0

let solve_part_2 file_channel =
  let all_lines = input_lines file_channel in
  let rec process_lines_2 arr =
    match arr with
    | [] -> 0
    | x :: xs -> (
        match line_adjustments_part_2 x with
        | Some x -> x + process_lines_2 xs
        | None -> process_lines_2 xs)
  in
  process_lines_2 all_lines
