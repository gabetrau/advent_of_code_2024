(* let input = "part1_input.txt" *)
let part2_input = "part2_input.txt"

let solve puzzle_input do_enabled =
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
      if comma_index > 4 then
        let rest = String.sub word 4 (comma_index - 4) in
        let num_one_length = String.length rest in
        let has_num_one =
          num_one_length >= 1 && num_one_length <= 3
          && String.to_seq rest |> List.of_seq |> is_number
        in
        if has_num_one && comma_index + 1 < right_index then
          let num_one = int_of_string rest in
          let rest2 =
            String.sub word (comma_index + 1) (right_index - comma_index - 1)
          in
          let num_two_length = String.length rest2 in
          let has_num_two =
            num_two_length >= 1 && num_two_length <= 3
            && String.to_seq rest2 |> List.of_seq |> is_number
          in
          if has_num_two then
            let num_two = int_of_string rest2 in
            num_one * num_two
          else 0
        else (
          Printf.printf "cow";
          0)
      else 0
    else 0
  in
  let check_do part do_enabled =
    match part with
    | x when String.starts_with ~prefix:"do()" x -> true
    | x when String.starts_with ~prefix:"don't()" x -> false
    | _ -> do_enabled
  in
  let rec check_input total line do_enabled =
    if String.length line <= 3 then (total, do_enabled)
    else if String.length line > 3 && String.length line <= 12 then
      let new_length = String.length line - 1 in
      if do_enabled then
        check_input
          (check_twelve line + total)
          (String.sub line 1 new_length)
          (check_do line do_enabled)
      else check_input total (String.sub line 1 new_length) do_enabled
    else
      let new_length = String.length line - 1 in
      let part = String.sub line 0 12 in
      if do_enabled then
        check_input
          (check_twelve part + total)
          (String.sub line 1 new_length)
          (check_do part do_enabled)
      else
        check_input total
          (String.sub line 1 new_length)
          (check_do part do_enabled)
  in
  check_input 0 puzzle_input do_enabled

let () =
  let ic = open_in part2_input in
  let print_result part item = Printf.printf "%s: %d\n" part item in
  let rec read_puzzle total do_enabled =
    try
      let line = input_line ic in
      match line with
      | "" -> total
      | _ ->
          let line_total = solve line do_enabled in
          read_puzzle (total + fst line_total) (snd line_total)
    with End_of_file -> total
  in
  try
    print_result "Part2" (read_puzzle 0 true);
    close_in ic
  with Sys_error _ -> close_in_noerr ic
