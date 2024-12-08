let input = "input.txt"

let solve puzzle_input =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let check_twelve word =
    if
      String.starts_with ~prefix:"mul(" word
      && String.length word >= 8
      && String.get word 4 |> is_digit
      && String.contains word ',' && String.contains word ')'
    then
      let rec is_number num_str =
        match num_str with
        | [] -> true
        | x :: _ when is_digit x |> not -> false
        | _ :: tail -> is_number tail
      in
      let comma_index = String.index word ',' in
      let right_index = String.index word ')' in
      let rest = String.sub word 4 comma_index in
      let num_one_length = String.length rest in
      let has_num_one =
        num_one_length >= 1 && num_one_length <= 3
        && String.to_seq rest |> List.of_seq |> is_number
      in
      if has_num_one && comma_index < right_index then
        let num_one = int_of_string rest in
        let rest2 = String.sub word comma_index right_index in
        let num_two_length = String.length rest2 in
        let has_num_two =
          num_two_length >= 1 && num_two_length <= 3
          && String.to_seq rest2 |> List.of_seq |> is_number
        in
        if has_num_two then
          let num_two = int_of_string rest2 in
          num_one * num_two
        else 0
      else 0
    else 0
  in
  let rec check_input total line =
    if String.length line <= 12 then check_twelve line + total
    else
      check_input
        (check_twelve (String.sub line 0 12) + total)
        (String.sub line 12 (String.length line))
  in
  check_input 0 puzzle_input

let () =
  let ic = open_in input in
  let print_result part item = Printf.printf "%s: %d\n" part item in
  let rec read_puzzle total =
    try
      let line = input_line ic in
      match line with
      | "" -> total
      | _ ->
          let line_total = solve line in
          read_puzzle (total + line_total)
    with End_of_file -> total
  in
  try
    print_result "Part1: " (read_puzzle 0);
    close_in ic
  with Sys_error _ -> close_in_noerr ic
