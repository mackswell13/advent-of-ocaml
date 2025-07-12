let file = "input.txt"
let ic = open_in file

let rec advent acc =
  try
    let line = input_line ic in
    advent (line :: acc)
  with End_of_file ->
    close_in ic;
    List.rev acc

let print_char_list clist =
  let str = String.of_seq (List.to_seq clist) in
  print_endline str

let input = advent []
let () = print_endline (List.hd input)
let split_input = List.map (fun x -> String.split_on_char ' ' x) input

let () =
  let first_line = List.hd split_input in
  List.iter print_endline first_line

let rec p_1 list res =
  match list with
  | [] -> res
  | hd :: tail -> (
      match hd with
      | [ "A"; "X" ] -> p_1 tail (res + 1 + 3)
      | [ "A"; "Y" ] -> p_1 tail (res + 2 + 6)
      | [ "A"; "Z" ] -> p_1 tail (res + 3)
      | [ "B"; "X" ] -> p_1 tail (res + 1)
      | [ "B"; "Y" ] -> p_1 tail (res + 2 + 3)
      | [ "B"; "Z" ] -> p_1 tail (res + 3 + 6)
      | [ "C"; "X" ] -> p_1 tail (res + 1 + 6)
      | [ "C"; "Y" ] -> p_1 tail (res + 2)
      | [ "C"; "Z" ] -> p_1 tail (res + 3 + 3)
      | _ -> p_1 list res)
;;

let () = 
  print_endline (string_of_int(p_1 split_input 0))

module RPS = struct
  type t =
    | Rock
    | Paper
    | Sicsors

  type result =
    | Win
    | Lose
    | Draw

  let of_string = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Sicsors 
    | _ -> assert false
  ;;
    
  let get_result = function
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> assert false
  ;;

  let get_win = function
    | Rock -> Paper
    | Paper -> Sicsors
    | Sicsors -> Rock
  ;;

  let get_loss = function
    | Rock -> Sicsors
    | Paper -> Rock
    | Sicsors -> Paper
  ;;

  let get_result_value = function
    | Win -> 6
    | Draw -> 3
    | Lose -> 0
  ;;

  let get_t_value= function
    | Rock -> 1 
    | Paper -> 2
    | Sicsors -> 3
  ;;
end

let line_helper theirs ours = 
  let them = RPS.of_string(theirs) in
  let result = RPS.get_result(ours) in
  let us = match result with
    | Win -> RPS.get_win(them)
    | Lose -> RPS.get_loss(them)
    | Draw -> them
  in
  (RPS.get_t_value(us) + RPS.get_result_value(result))
;;

let rec p_2 list res =
  match list with
  | [] -> res
  | hd :: tail -> (
    match hd with
    | [theirs; ours] -> p_2 tail (res + line_helper theirs ours)
    | _ -> assert false
  )
;;

let () = 
  print_endline (string_of_int(p_2 split_input 0))
  




