let file_channel = open_in "input.txt"
let games = Cube.solve_part_1 file_channel;;

print_int games;;
print_newline ()

let () = close_in file_channel
