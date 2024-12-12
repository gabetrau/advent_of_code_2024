type direction = Up | Down | Left | Right
type guard_snapshot = { spot : int * int; direction : direction; visited : int }

let rec read_map in_strm map =
  try
    let line = input_line in_strm in
    read_map in_strm ((String.to_seq line |> List.of_seq) :: map)
  with End_of_file -> List.rev map

let rec find_guard map coord =
  match map with
  | [] -> coord
  | hd :: tail -> (
      let guard_pos = List.find_index (fun c -> c = '^') hd in
      match guard_pos with
      | Some p -> (fst coord, p)
      | None -> find_guard tail (fst coord + 1, snd coord))

let mark_map map pos c =
  let row = List.nth map (fst pos) in
  let new_row = List.mapi (fun i d -> if i = snd pos then c else d) row in
  List.mapi (fun i r -> if i = fst pos then new_row else r) map

let rec find_distinct_spots map guard =
  let max_row = List.length map - 1 in
  let max_col = List.length (List.nth map 0) - 1 in
  if guard.visited > 4611686018427387000 then guard.visited
  else
    match guard.direction with
    | Up when fst guard.spot - 1 < 0 -> guard.visited
    | Down when fst guard.spot + 1 > max_row -> guard.visited
    | Left when snd guard.spot - 1 < 0 -> guard.visited
    | Right when snd guard.spot + 1 > max_col -> guard.visited
    | Up ->
        let row = List.nth map (fst guard.spot - 1) in
        let new_spot_val = List.nth row (snd guard.spot) in
        if new_spot_val = '#' then
          let new_guard =
            { spot = guard.spot; direction = Right; visited = guard.visited }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
        else if new_spot_val = 'X' then
          let new_guard =
            {
              spot = (fst guard.spot - 1, snd guard.spot);
              direction = guard.direction;
              visited = guard.visited;
            }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
        else
          let new_guard =
            {
              spot = (fst guard.spot - 1, snd guard.spot);
              direction = guard.direction;
              visited = guard.visited + 1;
            }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
    | Down ->
        let row = List.nth map (fst guard.spot + 1) in
        let new_spot_val = List.nth row (snd guard.spot) in
        if new_spot_val = '#' then
          let new_guard =
            { spot = guard.spot; direction = Left; visited = guard.visited }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
        else if new_spot_val = 'X' then
          let new_guard =
            {
              spot = (fst guard.spot + 1, snd guard.spot);
              direction = guard.direction;
              visited = guard.visited;
            }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
        else
          let new_guard =
            {
              spot = (fst guard.spot + 1, snd guard.spot);
              direction = guard.direction;
              visited = guard.visited + 1;
            }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
    | Left ->
        let row = List.nth map (fst guard.spot) in
        let new_spot_val = List.nth row (snd guard.spot - 1) in
        if new_spot_val = '#' then
          let new_guard =
            { spot = guard.spot; direction = Up; visited = guard.visited }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
        else if new_spot_val = 'X' then
          let new_guard =
            {
              spot = (fst guard.spot, snd guard.spot - 1);
              direction = guard.direction;
              visited = guard.visited;
            }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
        else
          let new_guard =
            {
              spot = (fst guard.spot, snd guard.spot - 1);
              direction = guard.direction;
              visited = guard.visited + 1;
            }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
    | Right ->
        let row = List.nth map (fst guard.spot) in
        let new_spot_val = List.nth row (snd guard.spot + 1) in
        if new_spot_val = '#' then
          let new_guard =
            { spot = guard.spot; direction = Down; visited = guard.visited }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
        else if new_spot_val = 'X' then
          let new_guard =
            {
              spot = (fst guard.spot, snd guard.spot + 1);
              direction = guard.direction;
              visited = guard.visited;
            }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard
        else
          let new_guard =
            {
              spot = (fst guard.spot, snd guard.spot + 1);
              direction = guard.direction;
              visited = guard.visited + 1;
            }
          in
          find_distinct_spots (mark_map map guard.spot 'X') new_guard

let rec find_loop_causing_obstacle map guard =
  let max_row = List.length map - 1 in
  let max_col = List.length (List.nth map 0) - 1 in
  match guard.direction with
  | Up when fst guard.spot - 1 < 0 -> 0
  | Down when fst guard.spot + 1 > max_row -> 0
  | Left when snd guard.spot - 1 < 0 -> 0
  | Right when snd guard.spot + 1 > max_col -> 0
  | Up ->
      let row = List.nth map (fst guard.spot - 1) in
      let new_spot_val = List.nth row (snd guard.spot) in
      if new_spot_val = '#' then
        let new_guard =
          { spot = guard.spot; direction = Right; visited = guard.visited }
        in
        let curr_row = List.nth map (fst guard.spot) in
        let curr_spot_val = List.nth curr_row (snd guard.spot) in
        if curr_spot_val = '-' then 1
        else if curr_spot_val = '.' then
          find_loop_causing_obstacle
            (mark_map map (fst guard.spot, snd guard.spot) '-')
            new_guard
        else find_loop_causing_obstacle map new_guard
      else
        let new_guard =
          {
            spot = (fst guard.spot - 1, snd guard.spot);
            direction = guard.direction;
            visited = guard.visited;
          }
        in
        find_loop_causing_obstacle map new_guard
  | Down ->
      let row = List.nth map (fst guard.spot + 1) in
      let new_spot_val = List.nth row (snd guard.spot) in
      if new_spot_val = '#' then
        let new_guard =
          { spot = guard.spot; direction = Left; visited = guard.visited }
        in
        let curr_row = List.nth map (fst guard.spot) in
        let curr_spot_val = List.nth curr_row (snd guard.spot) in
        if curr_spot_val = '_' then 1
        else if curr_spot_val = '.' then
          find_loop_causing_obstacle
            (mark_map map (fst guard.spot, snd guard.spot) '_')
            new_guard
        else find_loop_causing_obstacle map new_guard
      else
        let new_guard =
          {
            spot = (fst guard.spot + 1, snd guard.spot);
            direction = guard.direction;
            visited = guard.visited;
          }
        in
        find_loop_causing_obstacle map new_guard
  | Left ->
      let row = List.nth map (fst guard.spot) in
      let new_spot_val = List.nth row (snd guard.spot - 1) in
      if new_spot_val = '#' then
        let new_guard =
          { spot = guard.spot; direction = Up; visited = guard.visited }
        in
        let curr_row = List.nth map (fst guard.spot) in
        let curr_spot_val = List.nth curr_row (snd guard.spot) in
        if curr_spot_val = '<' then 1
        else if curr_spot_val = '.' then
          find_loop_causing_obstacle
            (mark_map map (fst guard.spot, snd guard.spot) '<')
            new_guard
        else find_loop_causing_obstacle map new_guard
      else
        let new_guard =
          {
            spot = (fst guard.spot, snd guard.spot - 1);
            direction = guard.direction;
            visited = guard.visited;
          }
        in
        find_loop_causing_obstacle map new_guard
  | Right ->
      let row = List.nth map (fst guard.spot) in
      let new_spot_val = List.nth row (snd guard.spot + 1) in
      if new_spot_val = '#' then
        let new_guard =
          { spot = guard.spot; direction = Down; visited = guard.visited }
        in
        let curr_row = List.nth map (fst guard.spot) in
        let curr_spot_val = List.nth curr_row (snd guard.spot) in
        if curr_spot_val = '>' then 1
        else if curr_spot_val = '.' then
          find_loop_causing_obstacle
            (mark_map map (fst guard.spot, snd guard.spot) '>')
            new_guard
        else find_loop_causing_obstacle map new_guard
      else
        let new_guard =
          {
            spot = (fst guard.spot, snd guard.spot + 1);
            direction = guard.direction;
            visited = guard.visited;
          }
        in
        find_loop_causing_obstacle map new_guard

let rec sum_row_results row total =
  match row with [] -> total | hd :: tail -> sum_row_results tail (total + hd)

let rec find_all_loop_obstacles map_trav map guard row_num total =
  match map_trav with
  | [] -> total
  | hd :: tail ->
      let results =
        List.mapi
          (fun i e ->
            if e = '#' || e = '^' then 0
            else
              find_loop_causing_obstacle (mark_map map (row_num, i) '#') guard)
          hd
      in
      find_all_loop_obstacles tail map guard (row_num + 1)
        (total + sum_row_results results 0)

let () =
  let output_answer part answer = Printf.printf "%s: %d\n" part answer in
  let in_strm = open_in "input_p1.txt" in
  let in_strm_2 = open_in "input_p2.txt" in
  try
    let map = read_map in_strm [] in
    let guard = { spot = find_guard map (0, 0); direction = Up; visited = 1 } in
    output_answer "part1" (find_distinct_spots map guard);
    close_in in_strm;
    let map_2 = read_map in_strm_2 [] in
    let guard_2 =
      { spot = find_guard map_2 (0, 0); direction = Up; visited = 1 }
    in
    output_answer "part2" (find_all_loop_obstacles map_2 map_2 guard_2 0 0);
    close_in in_strm_2
  with Sys_error _ ->
    close_in_noerr in_strm;
    close_in_noerr in_strm_2
