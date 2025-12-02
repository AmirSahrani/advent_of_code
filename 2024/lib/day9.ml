open In_channel

[@@@ocaml.warning "-26"]

let readdata ic =
  let out = input_line ic in
  match out with
  | None -> ""
  | Some x -> x
;;

let rec str_to_list str =
  let length = String.length str in
  match str with
  | "" -> []
  | x ->
    (int_of_char x.[0] - int_of_char '0') :: str_to_list (String.sub str 1 (length - 1))
;;

let rec repeat value count = if count > 0 then value :: repeat value (count - 1) else []

let rec gen_disk lst contains_data data_index =
  match lst, contains_data with
  | [], _ -> []
  | hd :: tl, false -> repeat None hd @ gen_disk tl true data_index
  | hd :: tl, true -> repeat (Some data_index) hd @ gen_disk tl false (data_index + 1)
;;

let rec remove_none lst =
  match lst with
  | [] -> []
  | None :: tl -> remove_none tl
  | Some x :: tl -> x :: remove_none tl
;;

let drop_lst n lst =
  let length = List.length lst in
  let out = List.filteri (fun i _ -> i < length - n) lst in
  out
;;

let fill_gaps lst =
  (* let lst_ref = ref lst in *)
  let possible = ref (remove_none (List.rev lst)) in
  List.fold_left_map
    (fun acc x ->
      match x with
      | None ->
        (try
           let hd = List.hd !possible in
           possible := List.tl !possible;
           acc + 1, hd
         with
         | Failure _ -> acc, 0)
      | Some x -> acc, x)
    0
    lst
;;

let find_n_consecutive_none arr n max =
  let is_n_none i =
    if i + n > max
    then false
    else List.init n (fun j -> arr.(i + j)) |> List.for_all (( = ) None)
  in
  try Some (Array.to_list arr |> List.mapi (fun i _ -> i) |> List.find is_n_none) with
  | Not_found -> None
;;

let fill_gaps_block (lst : int option list) blocks =
  let arr = Array.of_list lst in
  let arr_blocks = Array.of_list blocks in
  let max_id =
    List.fold_right
      (fun x acc ->
        match x with
        | None -> acc
        | Some n when n > acc -> n
        | _ -> acc)
      lst
      0
  in
  let rec aux id =
    if id < 0
    then ()
    else (
      let required, _ = arr_blocks.(id) in
      let required = required in
      let minimum =
        Array.find_index
          (fun x ->
            match x with
            | None -> false
            | Some y -> y = id)
          arr
      in
      let minimum =
        match minimum with
        | None -> 0
        | Some x -> x
      in
      match find_n_consecutive_none arr required minimum with
      | None -> aux (id - 1)
      | Some n ->
        Array.iteri (fun i x -> if x = Some id then arr.(i) <- None) arr;
        List.iter (fun i -> arr.(n + i) <- Some id) (List.init required (fun j -> j));
        aux (id - 1))
  in
  aux max_id;
  arr
;;

let part_1 data =
  let lst_data = str_to_list data in
  let disk = gen_disk lst_data true 0 in
  let n, filled = fill_gaps disk in
  let dropped = drop_lst n filled in
  let check_sum = List.mapi (fun i x -> i * x) dropped in
  let total = List.fold_right (fun x acc -> acc + x) check_sum 0 in
  Printf.printf "Part 1: %d\n" total
;;

let part_2 data =
  let lst_data = str_to_list data in
  let disk = gen_disk lst_data true 0 in
  let blocks =
    List.mapi (fun id size -> if id mod 2 = 0 then size, id / 2 else -1, -1) lst_data
  in
  let blocks = List.filter (fun (x, _) -> x <> -1) blocks in
  let filled = fill_gaps_block disk blocks in
  let check_sum =
    Array.mapi
      (fun i x ->
        match x with
        | Some x -> x * i
        | None -> 0)
      filled
  in
  let total = Array.fold_right (fun x acc -> acc + x) check_sum 0 in
  Printf.printf "Part 2: %d\n" total
;;

let () =
  let ic = open_in "data/input" in
  let data = readdata ic in
  part_1 data;
  part_2 data
;;
