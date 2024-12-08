let input = "input.txt"

let count_safe list =
  List.filter (fun i -> String.equal i "Safe") list |> List.length

let levels_are_safe dampen levels =
  let rec increase_check list =
    match list with
    | [] -> true
    | a :: b :: _ when b <= a || b - a < 1 || b - a > 3 -> false
    | _ :: rest -> increase_check rest
  in
  let rec decrease_check list =
    match list with
    | [] -> true
    | a :: b :: _ when a <= b || a - b < 1 || a - b > 3 -> false
    | _ :: rest -> decrease_check rest
  in
  let rec appended_list left_list right_list =
    match left_list with
    | [] -> right_list
    | hd :: rest -> appended_list rest (hd :: right_list)
  in
  let rec dampener_check left_list list =
    match list with
    | [] -> false
    | hd :: rest ->
        let dampened_list = appended_list left_list rest in
        if increase_check dampened_list || decrease_check dampened_list then
          true
        else dampener_check (hd :: left_list) rest
  in
  let level_nums =
    List.map (fun l -> int_of_string l) (String.split_on_char ' ' levels)
  in
  if
    increase_check level_nums || decrease_check level_nums
    || (dampen && dampener_check [] level_nums)
  then "Safe"
  else "Unsafe"

let eval_safety dampen list =
  let rec aux list safety_list =
    match list with
    | [] -> safety_list
    | levels :: rest -> aux rest (levels_are_safe dampen levels :: safety_list)
  in
  aux list []

let () =
  let ic = open_in input in
  let output_line part item =
    Printf.fprintf Stdlib.stdout "%s: %d\n" part item
  in
  let rec input_lines lists =
    try
      let line = input_line ic in
      match line with "" -> lists | _ -> line :: lists |> input_lines
    with End_of_file -> lists
  in
  try
    (* let safety_lines = input_lines [] |> List.rev |> eval_safety false in *)
    (* count_safe safety_lines |> output_line "Part1"; *)
    let dampened_safety_lines =
      input_lines [] |> List.rev |> eval_safety true
    in
    count_safe dampened_safety_lines |> output_line "Part2";
    close_in ic;
    close_out Stdlib.stdout
  with Sys_error _ ->
    close_in_noerr ic;
    close_out_noerr Stdlib.stdout
