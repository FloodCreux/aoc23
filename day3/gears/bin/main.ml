let file_channel = open_in "input.txt"
let result_1 = Gears.solve_part_1 file_channel;;

print_string "Part 1: ";;
print_int result_1;;
print_newline ()

let () = close_in file_channel
