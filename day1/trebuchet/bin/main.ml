let file_channel = open_in "input.txt"
let part_1 = Trebuchet.solve_part_1 file_channel;;

print_string "Part 1: ";;
print_int part_1;;
print_newline ();;
close_in file_channel

let file_channel = open_in "input.txt";;

print_endline "Solution for part 2:"

let part_2 = Trebuchet.solve_part_2 file_channel;;

print_string "Part 2: ";;
print_int part_2;;
print_newline ()

let () = close_in file_channel
