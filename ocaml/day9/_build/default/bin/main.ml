let read_line ins = try input_line ins with End_of_file -> ""

let calculate_amphipod line =
  let parse ind num =
    let rec create_spots ch n files =
      if n <= 0 then files else create_spots ch (n - 1) (ch :: files)
    in
    if ind = List.length line - 1 then []
    else if ind mod 2 = 0 then
      create_spots (string_of_int ind) (int_of_char num) []
    else create_spots "." (int_of_char num) []
  in
  List.mapi parse line |> List.flatten

let is_digit = function '0' .. '9' -> true | _ -> false

let reorder line =
  let rec find_first_digit start_ind ind =
    if ind <= start_ind then None
    else if String.exists is_digit line.(ind) then Some ind
    else find_first_digit start_ind (ind - 1)
  in
  let swap_free_space ind =
    let last_num_ind = find_first_digit ind (Array.length line - 1) in
    match last_num_ind with
    | None -> false
    | Some end_ind ->
        line.(ind) <- line.(end_ind);
        line.(end_ind) <- ".";
        true
  in
  let rec do_ro start_ind =
    if start_ind < Array.length line - 1 then
      if line.(start_ind) = "." then (
        let swapped = swap_free_space start_ind in
        if swapped then do_ro (start_ind + 1))
      else do_ro (start_ind + 1)
  in
  Printf.printf "%d\n%!" (Array.length line - 1);
  do_ro 0

let calculate_sum line =
  Array.fold_left
    (fun total e -> if e != "." then total + int_of_string e else total)
    0 line

let () =
  let ins = open_in "input.txt" in
  try
    let original_line = read_line ins in
    let line = calculate_amphipod (List.of_seq (String.to_seq original_line)) in
    Printf.printf "spots\n%!";
    let line_array = List.to_seq line |> Array.of_seq in
    reorder line_array;
    Printf.printf "part1: %d\n" (calculate_sum line_array);
    close_in ins
  with Sys_error _ -> close_in_noerr ins
