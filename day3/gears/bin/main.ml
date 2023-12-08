let file_channel = open_in "input.txt"
let result_1 = Gears.solve_part_1 file_channel;;

print_endline "Part 1:";;
List.iter (fun (x, _, _) -> Printf.printf "%s\n" x) result_1

let () = close_in file_channel
