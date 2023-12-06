let file_channel = open_in "input.txt"
let lines = Gears.input_lines file_channel;;

Gears.solve_part_1 lines

let () = close_in file_channel
