type point = {
  antenna : char;
  antinode : bool;
}

let rec read_in istrm lines =
  try
    let line = input_line istrm in
    if line = "" then lines else read_in istrm (line :: lines)
  with End_of_file -> lines

let mark_antinodes grid tenna1 tenna2 =
  let difx = fst tenna2 - fst tenna1 in
  let dify = snd tenna2 - snd tenna1 in
  let ant1 = (fst tenna1 - difx, snd tenna1 - dify) in
  let ant2 = (fst tenna2 + difx, snd tenna2 + dify) in
  if fst ant1 >= 0 && snd ant1 >= 0 && fst ant1 < Array.length grid && snd ant1 < Array.length grid.(0) then
    let tenna1_point = grid.(fst tenna1).(snd tenna1) in
    
    let ant1_point = grid.(fst ant1).(snd ant1) in
    let ant2_point = grid.(fst ant2).(snd ant2) in
    if ant1_point = '.' then
      grid.(fst ant1).(snd ant1) <- { antenna = tenna1_point.antenna ; antinode = true };
    else 
      grid.(fst ant1).(snd ant1) <- { antenna = ant1_point.antenna ; antinode = true };
    if grid.(fst ant2).(snd ant2) = '.' then
      grid.(fst ant2).(snd ant2) <- { antenna = tenna1_point.antenna ; antinode = true };
    else
      grid.(fst ant2).(snd ant2) <- { antenna = ant2_point.antenna ; antinode = true };
    grid

let solve_part_1 grid total =
  let rec count_antinodes grid =
    Array.iter (fun r ->
      Array.iter (fun c -> if c.)



let () =
  let istrm = open_in "input.txt" in
  try
    let lines = read_in istrm [] in
    let grid =
      Array.init_matrix (List.length lines)
        (List.nth lines 0 |> String.length)
        (fun x y ->
          let row = List.nth lines x in
          {
            antenna = String.get row y;
            antinode = false;
          })
    in
    Printf.printf "part2: %d" (solve_part_1 grid 0);
    close_in istrm
  with Sys_error _ -> close_in_noerr istrm
