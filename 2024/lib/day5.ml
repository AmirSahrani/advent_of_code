let read_lines filename =
  let f = open_in filename in
  let rec loop () =
    try
      let next = input_line f in
      match next with "" -> loop () | _ -> next :: loop ()
    with End_of_file ->
      close_in f;
      []
  in
  loop ()

let filter_list char str = String.contains str char

let order_from str =
  let x = String.split_on_char '|' str in
  match x with
  | [ a; b ] -> (int_of_string a, int_of_string b)
  | _ -> failwith "wrong string"

let report_from str =
  let x = String.split_on_char ',' str in
  List.map int_of_string x

let rec all_pairs_greater lst =
  match lst with
  | [] -> []
  | hd :: tl ->
      let all_pairs = List.map (fun x -> (hd, x)) tl in
      all_pairs @ all_pairs_greater tl

let valid_report restriction report =
  not
    (List.exists
       (fun (x, y) -> List.exists (fun (a, b) -> (a, b) = (y, x)) restriction)
       (all_pairs_greater report))

let middle_elem lst =
  let max = List.length lst in
  let middle = max / 2 in
  List.nth lst middle

let compare_restriction restrictions x y =
  if List.exists (fun (a, b) -> (a, b) = (y, x)) restrictions then 1 else 0

let correct_report restrictions report =
  List.sort (compare_restriction restrictions) report

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx

let () =
  let data = read_lines "data/input" in
  let restrictions =
    List.filter (filter_list '|') data |> List.map order_from
  in
  let reports =
    List.filter (fun x -> not (filter_list '|' x)) data |> List.map report_from
  in

  let valid_reports = List.filter (valid_report restrictions) reports in
  let invalid_reports =
    List.filter (fun x -> not (valid_report restrictions x)) reports
  in
  let corrected_reports =
    time List.map (correct_report restrictions) invalid_reports
  in

  let middle_values = List.map middle_elem valid_reports in
  let middle_correct = List.map middle_elem corrected_reports in
  let sum = List.fold_right ( + ) middle_values 0 in
  let sum_correct = List.fold_right ( + ) middle_correct 0 in
  print_string "Part 1: ";
  print_int sum;
  print_string "\n";
  print_string "Part 2: ";
  print_int sum_correct
