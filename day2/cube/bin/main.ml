let file_channel = open_in "input.txt"
let games = Cube.solve_part_1 file_channel;;

print_int games;;
Printf.printf "\n";;
close_in file_channel

let file_channel = open_in "input.txt"
let games_2 = Cube.solve_part_2 file_channel;;

print_int games_2

let () = close_in file_channel
