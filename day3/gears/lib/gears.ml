let input_lines stdin =
  let rec input lines =
    match try Some (input_line stdin) with End_of_file -> None with
    | Some line -> input (line :: lines)
    | None -> List.rev lines
  in
  input []

let get_rows lst = List.length lst
let get_cols lst = List.hd lst |> String.length

let create_matrix lst =
  let rows = get_rows lst in
  let cols = get_cols lst in
  let matrix = Array.make_matrix rows cols ' ' in
  let rec fill_matrix lst row =
    match lst with
    | [] -> ()
    | hd :: tl ->
        let rec fill_col col =
          if col < String.length hd then (
            matrix.(row).(col) <- hd.[col];
            fill_col (col + 1))
        in
        if row < rows then fill_col 0;
        fill_matrix tl (row + 1)
  in
  fill_matrix lst 0;
  matrix

let solve_part_1 lst =
  let matrix = create_matrix lst in
  let rec solve row solution =
    Array.iter
      (fun col ->
        print_char row.(col);
        print_int solution)
      row
  in
  Array.iter (fun row -> solve row 0) matrix
