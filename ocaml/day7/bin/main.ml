let rec read_in istrm calibrations =
  try
    let line = input_line istrm in
    if line = "" then calibrations else read_in istrm (line :: calibrations)
  with End_of_file -> calibrations

let find_calibration cal =
  let rec solve_line line total =
    match line with
    | [] -> total
    | _ :: [] -> total
    | operator :: num :: tail when operator = "||" ->
        let str_total = string_of_int total ^ num in
        solve_line tail (int_of_string str_total)
    | operator :: num :: tail when operator = "*" ->
        solve_line tail (total * int_of_string num)
    | operator :: num :: tail when operator = "+" ->
        solve_line tail (total + int_of_string num)
    | _ :: _ :: tail -> solve_line tail total
  in
  let rec calibrates problems sol =
    match problems with
    | [] -> false
    | p :: ptail -> (
        match p with
        | [] -> calibrates ptail sol
        | first_num :: fntail ->
            if solve_line fntail (int_of_string first_num) = sol then true
            else calibrates ptail sol)
  in
  let rec find_problem numbers number_of_operators problem =
    match numbers with
    | [] -> problem
    | hd :: [] -> problem @ [ hd ]
    | hd :: tail ->
        if number_of_operators mod 3 = 0 then
          find_problem tail (number_of_operators / 3) (problem @ [ hd; "+" ])
        else if number_of_operators mod 3 = 2 then
          find_problem tail (number_of_operators / 3) (problem @ [ hd; "||" ])
        else find_problem tail (number_of_operators / 3) (problem @ [ hd; "*" ])
  in
  let rec find_all_problems numbers num_of_operators problems =
    if num_of_operators < 0 then problems
    else
      find_all_problems numbers (num_of_operators - 1)
        (find_problem numbers num_of_operators [] :: problems)
  in
  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n ->
        let b = pow a (n / 2) in
        b * b * if n mod 2 = 0 then 1 else a
  in
  let line_elems = String.split_on_char ' ' cal in
  match line_elems with
  | [] -> 0
  | sol_with_colon :: numbers ->
      let sol =
        String.sub sol_with_colon 0 (String.length sol_with_colon - 1)
        |> int_of_string
      in
      let operator_num = List.length numbers - 1 in
      let problems = find_all_problems numbers (pow 3 operator_num - 1) [] in
      if calibrates problems sol then sol else 0

let rec solve_part_1 calibrations total =
  match calibrations with
  | [] -> total
  | hd :: tail -> solve_part_1 tail (total + find_calibration hd)

let () =
  let istrm = open_in "input.txt" in
  try
    let calibrations = read_in istrm [] in
    Printf.printf "part2: %d" (solve_part_1 calibrations 0);
    close_in istrm
  with Sys_error _ -> close_in_noerr istrm
