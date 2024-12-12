type point = {
  antenna : char;
  antinode : bool;
}

let rec read_in istrm lines =
  try
    let line = input_line istrm in
    if line = "" then lines else read_in istrm (line :: lines)
  with End_of_file -> lines

let solve_part_1 grid total =


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
