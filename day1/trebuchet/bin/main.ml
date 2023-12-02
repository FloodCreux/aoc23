let input_lines stdin =
    let rec input lines =
        match try Some(input_line stdin) with End_of_file -> None with
            Some line -> input (line :: lines)
        | None -> List.rev lines
    in input [];;

let line_adjustments line =
    let n = String.length line in
    let rec aux first last i =
        if i >= n then
            match (first, last) with
            | (Some f, Some l) -> Some (int_of_string (String.make 1 f ^ String.make 1 l))
            | _ -> None
        else
            let c = String.get line i in
            if Char.code c >= Char.code '0' && Char.code c <= Char.code '9' then
                match first with
                | None -> aux (Some c) (Some c) (i + 1)
                | Some _ -> aux first (Some c) (i + 1)
            else aux first last (i + 1)
    in
    aux None None 0;;

let process_lines arr =
    Array.map line_adjustments arr;;

let file_channel = open_in "input.txt";;
let all_lines = input_lines file_channel;;
let arr_lines = Array.of_list all_lines;;
let all_digits = process_lines arr_lines;;

let sum = Array.fold_left (fun acc x -> match x with 
    Some x -> acc + x
    | None -> acc) 0 all_digits;;

print_int sum;;
print_newline ();;

let () = close_in file_channel;;
