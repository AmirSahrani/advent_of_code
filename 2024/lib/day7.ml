open Printf

let read_data filename =
  let f = open_in filename in
  let rec loop () =
    try
      let next = input_line f in
      next :: loop ()
    with
    | End_of_file -> []
  in
  loop ()
;;

let split_data data = List.map (fun x -> String.split_on_char ':' x) data

let listToTuple data =
  match data with
  | [] -> "", ""
  | [ x; y ] -> x, y
  | _ -> failwith "wrong input"
;;

let split_numbers data =
  List.map int_of_string (String.split_on_char ' ' (String.trim data))
;;

let fix data = List.map (fun (x, y) -> int_of_string x, split_numbers y) data
let concat i j = int_of_string (string_of_int i ^ string_of_int j)

let check_possibility (target, sums) ops =
  let rec check left rhs =
    if left > target
    then false
    else (
      match rhs with
      | [] -> left = target
      | hd :: tl -> List.exists (fun op -> check (op left hd) tl) ops)
  in
  match sums with
  | [] -> false
  | hd :: tl -> check hd tl
;;

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx
;;

let () =
  let data = read_data "data/input" in
  let split = List.map listToTuple (split_data data) in
  let fixed = fix split in
  let valid_part1 =
    time (List.filter (fun x -> check_possibility x [ ( + ); ( * ) ])) fixed
  in
  let valid_part2 =
    time (List.filter (fun x -> check_possibility x [ ( + ); ( * ); concat ])) fixed
  in
  let sum = List.fold_right (fun (x, _) acc -> x + acc) valid_part1 0 in
  let sum2 = List.fold_right (fun (x, _) acc -> x + acc) valid_part2 0 in
  printf "Part 1: %d \n" sum;
  printf "Part 2: %d \n" sum2
;;
