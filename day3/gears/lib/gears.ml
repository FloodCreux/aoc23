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

let check_first_row row next_row =
  let rec check row next_row col valid =
    if col >= Array.length row then valid
    else
      let item = Array.get row col in
      if item = '.' then check row next_row (col + 1) valid
      else
        let valid =
          if col > 0 && col < Array.length row then
            let left = Array.get row (col - 1) in
            let bottom_left = Array.get next_row (col - 1) in
            let bottom = Array.get next_row col in
            let bottom_right = Array.get next_row (col + 1) in
            valid || left != '.' || bottom_left != '.' || bottom != '.'
            || bottom_right != '.'
          else if col = 0 then
            let bottom_left = Array.get next_row (col - 1) in
            let bottom = Array.get next_row col in
            let bottom_right = Array.get next_row (col + 1) in
            valid || bottom_left != '.' || bottom != '.' || bottom_right != '.'
          else if col = Array.length row then
            let left = Array.get row (col - 1) in
            let bottom_left = Array.get next_row (col - 1) in
            let bottom = Array.get next_row col in
            valid || left != '.' || bottom_left != '.' || bottom != '.'
          else valid
        in
        check row next_row (col + 1) valid
  in
  check row next_row 0 false

let solve_part_1 lst =
  let matrix = create_matrix lst in
  let solve row _ = check_first_row row (Array.get matrix 1) in
  Array.mapi (fun _ row -> solve row 0) matrix
