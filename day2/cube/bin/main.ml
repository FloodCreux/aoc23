let file_channel = open_in "input.txt";;

Cube.solve_part_1 file_channel;;
print_newline ()

let () = close_in file_channel
