open Printf

let read_lines filename =
  let f = open_in filename in
  let rec loop () =
    try
      let next = input_line f in
      next :: loop ()
    with End_of_file ->
      close_in f;
      []
  in
  loop ()

let find_valid_mul =
  Str.regexp
    "mul(\\([0-9]\\|[0-9][0-9]\\|[0-9][0-9][0-9]\\)\\,\\([0-9]\\|[0-9][0-9]\\|[0-9][0-9][0-9]\\))"

let find_valid_mul_do_don't =
  Str.regexp
    "do()\\|don't()\\|mul(\\([0-9]\\|[0-9][0-9]\\|[0-9][0-9][0-9]\\)\\,\\([0-9]\\|[0-9][0-9]\\|[0-9][0-9][0-9]\\))"

let search_mul_forward str =
  let rec find_all_matches pos acc =
    try
      let match_pos = Str.search_forward find_valid_mul str pos in
      let matched = Str.matched_string str in
      find_all_matches (match_pos + String.length matched) (matched :: acc)
    with Not_found -> acc
  in
  find_all_matches 0 []

let search_mul_forward_do str =
  let rec find_all_matches pos acc add =
    try
      let match_pos = Str.search_forward find_valid_mul_do_don't str pos in
      let matched = Str.matched_string str in
      match matched with
      | "do()" -> find_all_matches (match_pos + String.length matched) acc true
      | "don't()" ->
          find_all_matches (match_pos + String.length matched) acc false
      | _ ->
          if add then
            find_all_matches
              (match_pos + String.length matched)
              (matched :: acc) add
          else find_all_matches (match_pos + String.length matched) acc add
    with Not_found -> acc
  in
  find_all_matches 0 [] true

let rec join sep = function
  | [] -> ""
  | [ str ] -> str
  | "" :: strs -> join sep strs
  | str :: strs -> str ^ sep ^ join sep strs

let mul_replace str =
  let mul_pattern = Str.regexp "mul(\\([0-9]+,[0-9]+\\))" in
  Str.global_replace mul_pattern "\\1" str

let prod lst = List.fold_right ( * ) lst 1

let prod matches =
  let multiplied =
    List.map
      (fun x ->
        List.map int_of_string (String.split_on_char ',' (mul_replace x)))
      matches
  in
  List.map prod multiplied

let () =
  let data = join " " (read_lines "data/input") in
  let _ = search_mul_forward data in
  let matches = search_mul_forward_do data in
  let prod_list = prod matches in
  let sum = List.fold_right ( + ) prod_list 0 in
  printf "Total: %d" sum
