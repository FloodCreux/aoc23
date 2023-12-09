let file_channel = open_in "input.txt";;

print_int (Scratch.solve_part_1 file_channel)

let () = close_in file_channel
