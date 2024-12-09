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

let correct_input rules input =
  let rec compare criteria left right =
    match criteria with
    | [] -> 0
    | hd :: _ when fst hd = left && snd hd = right -> -1
    | hd :: _ when fst hd = right && snd hd = left -> 1
    | _ :: tail -> compare tail left right
  in
  let sorted_input = List.sort (fun x y -> compare rules x y) input in
  List.nth sorted_input (List.length sorted_input / 2)

let check_update_p2 rules input =
  let check_rule rule =
    let left_num_in = List.find_index (fun i -> i = fst rule) input in
    let right_num_in = List.find_index (fun i -> i = snd rule) input in
    match left_num_in with
    | None -> true
    | Some l -> ( match right_num_in with None -> true | Some r -> l < r)
  in
  let fails = List.exists (fun r -> check_rule r = false) rules in
  match fails with true -> correct_input rules input | false -> 0

let rec calc_answer_p2 rules updates total =
  match updates with
  | [] -> total
  | x :: tail -> calc_answer_p2 rules tail (total + check_update_p2 rules x)

let () =
  let ic = open_in "input.txt" in
  let ic2 = open_in "input_p2.txt" in
  let print_result part result = Printf.printf "%s: %d\n" part result in
  try
    let rules = read_rules ic [] in
    let updates = read_updates ic [] in
    print_result "part 1" (calc_answer rules updates 0);
    let rules_p2 = read_rules ic2 [] in
    let updates_p2 = read_updates ic2 [] in
    print_result "part 2" (calc_answer_p2 rules_p2 updates_p2 0);
    close_in ic
  with Sys_error _ -> close_in_noerr ic
