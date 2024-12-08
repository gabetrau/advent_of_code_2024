let part1_input = "p1_input.txt"
let part2_input = "p2_input.txt"

type direction = Positive | Negative
type xmas_pattern = M_TOP | M_LEFT | M_RIGHT | M_BOTTOM

let rec format_input ic input =
  try
    let line = input_line ic in
    match line with
    | "" -> input
    | _ -> format_input ic ((String.to_seq line |> Array.of_seq) :: input)
  with End_of_file -> input

let create_matrix finput =
  let row_num = List.length finput in
  let col_num = try List.nth finput 0 |> Array.length with Failure _ -> 0 in
  let m = Array.make_matrix row_num col_num '.' in
  for i = 0 to Array.length m - 1 do
    m.(i) <- List.nth finput (List.length finput - 1 - i)
  done;
  m

let horizontal_check m x y dir =
  match dir with
  | Positive ->
      if x < 0 || x + 3 >= Array.length m || y < 0 || y >= Array.length m.(0)
      then false
      else m.(x + 1).(y) = 'M' && m.(x + 2).(y) = 'A' && m.(x + 3).(y) = 'S'
  | Negative ->
      if x - 3 < 0 || x >= Array.length m || y < 0 || y >= Array.length m.(0)
      then false
      else m.(x - 1).(y) = 'M' && m.(x - 2).(y) = 'A' && m.(x - 3).(y) = 'S'

let vertical_check m x y dir =
  match dir with
  | Positive ->
      if x < 0 || x >= Array.length m || y < 0 || y + 3 >= Array.length m.(0)
      then false
      else m.(x).(y + 1) = 'M' && m.(x).(y + 2) = 'A' && m.(x).(y + 3) = 'S'
  | Negative ->
      if x < 0 || x >= Array.length m || y - 3 < 0 || y >= Array.length m.(0)
      then false
      else m.(x).(y - 1) = 'M' && m.(x).(y - 2) = 'A' && m.(x).(y - 3) = 'S'

let diagonal_check m x y dir =
  match dir with
  | Positive ->
      if
        x < 0 || x + 3 >= Array.length m || y < 0 || y + 3 >= Array.length m.(0)
      then false
      else
        m.(x + 1).(y + 1) = 'M'
        && m.(x + 2).(y + 2) = 'A'
        && m.(x + 3).(y + 3) = 'S'
  | Negative ->
      if
        x - 3 < 0 || x >= Array.length m || y - 3 < 0 || y >= Array.length m.(0)
      then false
      else
        m.(x - 1).(y - 1) = 'M'
        && m.(x - 2).(y - 2) = 'A'
        && m.(x - 3).(y - 3) = 'S'

let diagonal_down_check m x y dir =
  match dir with
  | Positive ->
      if
        x < 0 || x + 3 >= Array.length m || y - 3 < 0 || y >= Array.length m.(0)
      then false
      else
        m.(x + 1).(y - 1) = 'M'
        && m.(x + 2).(y - 2) = 'A'
        && m.(x + 3).(y - 3) = 'S'
  | Negative ->
      if
        x - 3 < 0 || x >= Array.length m || y < 0 || y + 3 >= Array.length m.(0)
      then false
      else
        m.(x - 1).(y + 1) = 'M'
        && m.(x - 2).(y + 2) = 'A'
        && m.(x - 3).(y + 3) = 'S'

let solve_puzzle m =
  if Array.length m = 0 || Array.length m.(0) = 0 then 0
  else
    let check_letter l x y =
      if l != 'X' then 0
      else
        let word_val w = match w with true -> 1 | false -> 0 in
        (horizontal_check m x y Positive |> word_val)
        + (horizontal_check m x y Negative |> word_val)
        + (vertical_check m x y Positive |> word_val)
        + (vertical_check m x y Negative |> word_val)
        + (diagonal_check m x y Positive |> word_val)
        + (diagonal_check m x y Negative |> word_val)
        + (diagonal_down_check m x y Positive |> word_val)
        + (diagonal_down_check m x y Negative |> word_val)
    in
    let rec traverse coord max_x max_y total =
      match coord with
      | x, y when y > max_y || (x > max_x && y >= max_y) -> total
      | x, y when x > max_x -> traverse (0, y + 1) max_x max_y total
      | x, y ->
          traverse (x + 1, y) max_x max_y (total + check_letter m.(x).(y) x y)
    in
    traverse (0, 0) (Array.length m - 1) (Array.length m.(0) - 1) 0

let mas_check m x y mtype =
  if
    x - 1 < 0
    || x + 1 >= Array.length m
    || y - 1 < 0
    || y + 1 >= Array.length m.(0)
  then false
  else
    match mtype with
    | M_TOP ->
        m.(x - 1).(y + 1) = 'M'
        && m.(x + 1).(y + 1) = 'M'
        && m.(x - 1).(y - 1) = 'S'
        && m.(x + 1).(y - 1) = 'S'
    | M_LEFT ->
        m.(x - 1).(y + 1) = 'M'
        && m.(x + 1).(y + 1) = 'S'
        && m.(x - 1).(y - 1) = 'M'
        && m.(x + 1).(y - 1) = 'S'
    | M_RIGHT ->
        m.(x - 1).(y + 1) = 'S'
        && m.(x + 1).(y + 1) = 'M'
        && m.(x - 1).(y - 1) = 'S'
        && m.(x + 1).(y - 1) = 'M'
    | M_BOTTOM ->
        m.(x - 1).(y + 1) = 'S'
        && m.(x + 1).(y + 1) = 'S'
        && m.(x - 1).(y - 1) = 'M'
        && m.(x + 1).(y - 1) = 'M'

let solve_puzzle_part2 m =
  if Array.length m = 0 || Array.length m.(0) = 0 then 0
  else
    let check_letter l x y =
      if l != 'A' then 0
      else
        let word_val w = match w with true -> 1 | false -> 0 in
        (mas_check m x y M_TOP |> word_val)
        + (mas_check m x y M_LEFT |> word_val)
        + (mas_check m x y M_RIGHT |> word_val)
        + (mas_check m x y M_BOTTOM |> word_val)
    in
    let rec traverse coord max_x max_y total =
      match coord with
      | x, y when y > max_y || (x > max_x && y >= max_y) -> total
      | x, y when x > max_x -> traverse (0, y + 1) max_x max_y total
      | x, y ->
          traverse (x + 1, y) max_x max_y (total + check_letter m.(x).(y) x y)
    in
    traverse (0, 0) (Array.length m - 1) (Array.length m.(0) - 1) 0

let () =
  let ic = open_in part1_input in
  let ic2 = open_in part2_input in
  let print_result part item = Printf.printf "%s: %d\n" part item in
  try
    let m = format_input ic [] |> create_matrix in
    let m2 = format_input ic2 [] |> create_matrix in
    Printf.printf "\n\n";
    print_result "Part1" (solve_puzzle m);
    print_result "Part2" (solve_puzzle_part2 m2);
    close_in ic;
    close_in ic2
  with Sys_error _ ->
    close_in_noerr ic;
    close_in_noerr ic2
