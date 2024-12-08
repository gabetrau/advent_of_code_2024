let rec read_rules ic print_rules =
  try
    let line = input_line ic in
    let has_bar = String.contains line '|' in
    if has_bar = false then print_rules
    else
      let split_rule = String.split_on_char '|' line in
      let lrule = List.hd split_rule in
      let rrule = List.nth split_rule (List.length split_rule - 1) in
      read_rules ic ((int_of_string lrule, int_of_string rrule) :: print_rules)
  with End_of_file -> print_rules

let rec read_updates ic print_updates =
  try
    let line = input_line ic in
    if line = "" then print_updates
    else
      let updates_list = String.split_on_char ',' line in
      read_updates ic
        (List.map (fun u -> int_of_string u) updates_list :: print_updates)
  with End_of_file -> print_updates

let check_update rules input =
  let check_rule rule =
    let left_num_in = List.find_index (fun i -> i = fst rule) input in
    let right_num_in = List.find_index (fun i -> i = snd rule) input in
    match left_num_in with
    | None -> true
    | Some l -> ( match right_num_in with None -> true | Some r -> l < r)
  in
  let fails = List.exists (fun r -> check_rule r = false) rules in
  match fails with true -> 0 | false -> List.nth input (List.length input / 2)

let rec calc_answer rules updates total =
  match updates with
  | [] -> total
  | x :: tail -> calc_answer rules tail (total + check_update rules x)

let () =
  let ic = open_in "input.txt" in
  let print_result part result = Printf.printf "%s: %d\n" part result in
  try
    let rules = read_rules ic [] in
    let updates = read_updates ic [] in
    print_result "part 1" (calc_answer rules updates 0);
    close_in ic
  with Sys_error _ -> close_in_noerr ic
