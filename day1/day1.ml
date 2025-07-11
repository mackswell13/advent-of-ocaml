let file = "input.txt"
let ic = open_in file

let rec advent acc =
  try
    let line = input_line ic in
    advent (line :: acc)
  with End_of_file ->
    close_in ic;
    List.rev acc

let input = advent []

(* Print a list of strings 
let () = List.iter print_endline input;;
*)

let rec group in_list result =
  match in_list with
  | [] -> result
  | "" :: rest -> group rest (0 :: result)
  | line :: rest ->
      group rest
        (match result with
        | [] -> [ int_of_string line ]
        | hd :: tail -> (hd + int_of_string line) :: tail)

let grouped = group input []

let rec max_of_list list cur =
  match list with [] -> cur | hd :: tail -> max_of_list tail (max hd cur)

let rec top_three list (m1, m2, m3) =
  match list with
  | [] -> (m1, m2, m3)
  | hd :: tail ->
      top_three tail
        (match (m1, m2, m3) with
        | m1, m2, _ when hd > m3 -> (m1, m2, hd)
        | m1, _, m3 when hd > m2 -> (m1, hd, m3)
        | _, m2, m3 when hd > m1 -> (hd, m2, m3)
        | _ -> (m1, m2, m3))

let () =
  let out = max_of_list grouped 0 in
  print_endline (string_of_int out)

let () =
  let m1, m2, m3 = top_three grouped (0, 0, 0) in
  print_endline (string_of_int (m1 + m2 + m3))
