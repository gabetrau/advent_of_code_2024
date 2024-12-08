let part1_input = "part1_input.txt"
let part1_output = "part1_output.txt"
let part2_input = "part2_input.txt"
let part2_output = "part2_output.txt"

let sort_lists_and_subtract lists =
  let separated_lists = List.split lists in
  let list_one = fst separated_lists |> List.sort compare in
  let list_two = snd separated_lists |> List.sort compare in
  List.map2 (fun a b -> b - a |> abs) list_two list_one
;;

let part1 ic =
  let rec aux lists =
    try
      let line = input_line ic in
      match line with
      | "" -> lists
      | _ ->
        let items = String.split_on_char ' ' line in
        let id_one = List.hd items in
        let id_two = List.nth items (List.length items - 1) in
        (int_of_string id_one, int_of_string id_two) :: lists |> aux
    with
    | End_of_file -> lists
  in
  aux [] |> List.rev |> sort_lists_and_subtract
;;

let calc_sim_score left_list right_list =
  let rec aux sim left_list right_list =
    match left_list with
    | [] -> sim
    | id_one :: rest ->
      aux
        (sim
         + (id_one
            * (List.filter (fun id_two -> id_two == id_one) right_list |> List.length)))
        rest
        right_list
  in
  aux 0 left_list right_list
;;

let part2 ic =
  let rec aux lists =
    try
      let line = input_line ic in
      match line with
      | "" -> lists
      | _ ->
        let items = String.split_on_char ' ' line in
        let id_one = List.hd items in
        let id_two = List.nth items (List.length items - 1) in
        (int_of_string id_one, int_of_string id_two) :: lists |> aux
    with
    | End_of_file -> lists
  in
  let combined = aux [] |> List.rev in
  match List.split combined with
  | left_list, right_list -> calc_sim_score left_list right_list
;;

let () =
  print_endline "part1";
  let oc = open_out part1_output in
  let ic = open_in part1_input in
  let output_line item = Printf.fprintf oc "%d\n" item in
  (try
     List.fold_left ( + ) 0 (part1 ic) |> output_line;
     close_in ic;
     close_out oc
   with
   | Sys_error _ ->
     close_in_noerr ic;
     close_out_noerr oc);
  print_endline "part2";
  let oc = open_out part2_output in
  let ic = open_in part2_input in
  let output_line item = Printf.fprintf oc "%d\n" item in
  try
    part2 ic |> output_line;
    close_in ic;
    close_out oc
  with
  | Sys_error _ ->
    close_in_noerr ic;
    close_out_noerr oc
;;
