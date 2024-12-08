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

let find_failed_rule rules input =
  let check_rule rule =
    let left_num_in = List.find_index (fun i -> i = fst rule) input in
    let right_num_in = List.find_index (fun i -> i = snd rule) input in
    match left_num_in with
    | None -> true
    | Some l -> ( match right_num_in with None -> true | Some r -> l < r)
  in
  List.find_opt (fun r -> check_rule r = false) rules

let rec calc_answer rules updates total =
  match updates with
  | [] -> total
  | x :: tail -> calc_answer rules tail (total + check_update rules x)

let find_sorted_middle rules sorted lin rin =
  let swap u v n = match n with
     |x when x = u -> v
     |x when x = v -> u
     |_ -> n
  in
  let list_swap l u v = List.mapi (fun i -> fun _ -> swap (i - 1) i) l in
  let rec check_right counter =
    match check_update rules (list_swap sorted ) 



let rec correct_input rules input sorted_rules =
  match find_failed_rule rules input with
  | None -> List.nth input (List.length input / 2)
  | Some r -> (
      let left = fst r in
      let right = snd r in
      let left_in = List.find_index (fun l -> l = left) sorted_rules in
      let right_in = List.find_index (fun r -> r = right) sorted_rules in
      match (left_in, right_in) with
      | None, None ->
          let new_input =
            left :: right
            :: List.filter (fun i -> i != left && i != right) input
          in
          correct_input rules new_input [ left; right ]
      | Some _, None ->
          let new_sorted_rules = sorted_rules @ [ right ] in
          let new_input =
            new_sorted_rules @ List.filter (fun i -> i != right) input
          in
          correct_input rules new_input new_sorted_rules
      | None, Some _ ->
          let new_sorted_rules = left :: sorted_rules in
          let new_input =
            new_sorted_rules @ List.filter (fun i -> i != left) input
          in
          correct_input rules new_input new_sorted_rules
      | Some lin, Some rin ->
          let left_sr = List.filteri (fun i -> fun _ -> i < lin) sorted_rules in
          let right_sr = List.filteri (fun i -> fun _ -> i > rin) sorted_rules in
          let middle_sr = find_sorted_middle sorted_rules lin rin in
          let new_sorted_rules = left_sr @ middle_sr @ right_sr in
          let dc_pages =
            List.filter (fun p -> List.mem p new_sorted_rules |> not) input
          in
          let new_input = new_sorted_rules @ dc_pages in
          correct_input rules new_input new_sorted_rules)

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
    print_result "part 2" (calc_answer rules updates 0);
    close_in ic
  with Sys_error _ -> close_in_noerr ic
