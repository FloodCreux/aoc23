let file_channel = open_in "input.txt"
let lines = Gears.input_lines file_channel
let matrix = Gears.create_matrix lines
let () = close_in file_channel
