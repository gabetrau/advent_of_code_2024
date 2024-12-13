type point = { antenna : char; antinode : bool }

let rec read_in istrm lines =
  try
    let line = input_line istrm in
    if line = "" then lines else read_in istrm (line :: lines)
  with End_of_file -> lines

let mark_antinodes grid t1x t1y t2x t2y =
  let aux antx anty =
    let curr_point = grid.(antx).(anty) in
    if curr_point.antenna = '.' || curr_point.antenna = '#' then
      grid.(antx).(anty) <- { antenna = '#'; antinode = true }
    else grid.(antx).(anty) <- { antenna = curr_point.antenna; antinode = true }
  in
  let point1 = grid.(t1x).(t1y) in
  let point2 = grid.(t2x).(t2y) in
  grid.(t1x).(t1y) <- { antenna = point1.antenna; antinode = true };
  grid.(t2x).(t2y) <- { antenna = point2.antenna; antinode = true };
  let difx = t2x - t1x in
  let dify = t2y - t1y in
  let rec signal tx ty c dx dy up =
    let antx = if up then tx + (c * dx) else tx - (c * dx) in
    let anty = if up then ty + (c * dy) else ty - (c * dy) in
    if
      antx >= 0
      && antx < Array.length grid
      && anty >= 0
      && anty < Array.length grid.(0)
    then (
      aux antx anty;
      signal tx ty (c + 1) dx dy up)
  in
  signal t1x t1y 1 difx dify false;
  signal t2x t2y 1 difx dify true

let solve_part_1 grid =
  let count_antinodes grid =
    Array.fold_left
      (fun total r ->
        total
        + Array.fold_left
            (fun row_total c -> if c.antinode then row_total + 1 else row_total)
            0 r)
      0 grid
  in
  let check_rest_of_grid grid t1x t1y =
    for i = t1x to Array.length grid - 1 do
      if i = t1x then
        for j = t1y + 1 to Array.length grid.(0) - 1 do
          if grid.(i).(j).antenna = grid.(t1x).(t1y).antenna then
            mark_antinodes grid t1x t1y i j
        done
      else
        for j = 0 to Array.length grid.(0) - 1 do
          if grid.(i).(j).antenna = grid.(t1x).(t1y).antenna then
            mark_antinodes grid t1x t1y i j
        done
    done
  in
  Array.iteri
    (fun i r ->
      Array.iteri
        (fun j p ->
          if p.antenna != '.' && p.antenna != '#' then
            check_rest_of_grid grid i j)
        r)
    grid;
  count_antinodes grid

let () =
  let istrm = open_in "input.txt" in
  try
    let lines = read_in istrm [] in
    let grid =
      Array.init_matrix (List.length lines)
        (List.nth lines 0 |> String.length)
        (fun x y ->
          let row = List.nth lines x in
          { antenna = String.get row y; antinode = false })
    in
    Printf.printf "part2: %d" (solve_part_1 grid);
    close_in istrm
  with Sys_error _ -> close_in_noerr istrm
