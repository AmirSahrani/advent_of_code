
let list_data data = List.map (fun x -> Str.split (Str.regexp "[ ]+") x) data
let int_list data = List.map (fun x -> List.map int_of_string x) data

let rec take n lst =
  match lst with
  | [] -> []
  | _ when n <= 0 -> []
  | hd :: tl -> hd :: take (n - 1) tl

let remove i lst =
  let rec aux idx = function
    | [] -> []
    | hd :: tl -> if idx = i then tl else hd :: aux (idx + 1) tl
  in
  aux 0 lst

let first_n_minus_one lst =
  let n = List.length lst in
  if n <= 1 then [] else take (n - 1) lst

let is_sorted op lst =
  match lst with
  | [] | [ _ ] -> true
  | _ ->
      List.for_all
        (fun (x, y) -> op x y)
        (List.combine (first_n_minus_one lst) (List.tl lst))

let diff lst =
  List.map2 (fun x y -> abs (x - y)) (first_n_minus_one lst) (List.tl lst)

let diff_leq_three lst = not (List.exists (fun x -> x > 3) (diff lst))
let is_ascending lst = is_sorted ( < ) lst
let is_descending lst = is_sorted ( > ) lst
let int_of_bool b = if b then 1 else 0

let is_safe report =
  let direct = is_ascending report || is_descending report in
  let diff = diff_leq_three report in
  direct && diff

let is_tolerant_safe report =
  let n = List.length report in
  List.exists
    (fun i ->
      let dropped_report = remove i report in
      is_safe dropped_report)
    (List.init n (fun x -> x))

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx

let solve text =
  let data = time int_list (list_data (text)) in
  let safe_reports = time List.map is_safe data in
  let tolerant_reports = time List.map is_tolerant_safe data in

  let number_of_safe_reports =
    List.fold_right ( + ) (List.map int_of_bool safe_reports) 0
  in
  let number_of_tolerant_reports =
    List.fold_right ( + ) (List.map int_of_bool tolerant_reports) 0
  in
  (number_of_safe_reports, number_of_tolerant_reports)
