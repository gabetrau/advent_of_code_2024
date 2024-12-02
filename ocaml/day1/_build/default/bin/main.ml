let input = "input.txt"
let output = "output.txt"

let read_lines ic =
  let rec aux lists =
    try
      let line = input_line ic in
      match line with
      | "" -> lists
      | _ -> let items = String.split_on_char ' ' line in
        let id_one = List.hd items in
        let id_two = List.nth items (List.length items - 1) in
        ((int_of_string id_one + int_of_string id_two) :: lists) |> aux
    with End_of_file -> lists
  in
  aux [] |> List.rev

let () =
  let oc = open_out output in
  let ic = open_in input in
  let output_line item = Printf.fprintf oc "%d\n" item in
  try
    List.iter output_line (read_lines ic);
    close_in ic;
    close_out oc
  with e ->
    close_in_noerr ic;
    close_out_noerr oc;
    raise e
