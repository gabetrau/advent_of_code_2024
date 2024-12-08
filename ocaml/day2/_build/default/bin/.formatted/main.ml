let input = "input.txt"

let () =
  let ic = open_in input in
  let output_line item = Printf.fprintf Stdlib.stdout "%d\n" item in
  try
    close_in ic;
    close_out Stdlib.stdout
  with Sys_error _ ->
    close_in_noerr ic;
    close_out_noerr Stdlib.stdout
