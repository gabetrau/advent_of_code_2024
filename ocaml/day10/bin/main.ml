


let () =
  let output_result part item = Printf.printf "%s: %d\n" in
  let ic = open_in input.txt in
  try

  with Sys_error _ -> close_in_noerr ic
