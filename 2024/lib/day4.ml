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

let check_right lst_str line pos =
  if pos < String.length (List.nth lst_str line) - 3 then
    String.sub (List.nth lst_str line) pos 4
  else ""

let check_left lst_str line pos =
  if pos >= 3 then
    Char.escaped (List.nth lst_str line).[pos]
    ^ Char.escaped (List.nth lst_str line).[pos - 1]
    ^ Char.escaped (List.nth lst_str line).[pos - 2]
    ^ Char.escaped (List.nth lst_str line).[pos - 3]
  else ""

let check_up lst_str line pos =
  if line >= 3 then
    Char.escaped (List.nth lst_str line).[pos]
    ^ Char.escaped (List.nth lst_str (line - 1)).[pos]
    ^ Char.escaped (List.nth lst_str (line - 2)).[pos]
    ^ Char.escaped (List.nth lst_str (line - 3)).[pos]
  else ""

let check_down lst_str line pos =
  if line < List.length lst_str - 3 then
    Char.escaped (List.nth lst_str line).[pos]
    ^ Char.escaped (List.nth lst_str (line + 1)).[pos]
    ^ Char.escaped (List.nth lst_str (line + 2)).[pos]
    ^ Char.escaped (List.nth lst_str (line + 3)).[pos]
  else ""

let check_diag_up_right lst_str line pos =
  if pos < String.length (List.nth lst_str line) - 3 && line - 3 >= 0 then
    let out =
      Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line - 1)).[pos + 1]
      ^ Char.escaped (List.nth lst_str (line - 2)).[pos + 2]
      ^ Char.escaped (List.nth lst_str (line - 3)).[pos + 3]
    in
    out
  else ""

let check_diag_up_left lst_str line pos =
  if pos >= 3 && line >= 3 then
    let out =
      Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line - 1)).[pos - 1]
      ^ Char.escaped (List.nth lst_str (line - 2)).[pos - 2]
      ^ Char.escaped (List.nth lst_str (line - 3)).[pos - 3]
    in
    out
  else ""

let check_diag_down_right lst_str line pos =
  if
    pos < String.length (List.nth lst_str line) - 3
    && line < List.length lst_str - 3
  then
    let out =
      Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line + 1)).[pos + 1]
      ^ Char.escaped (List.nth lst_str (line + 2)).[pos + 2]
      ^ Char.escaped (List.nth lst_str (line + 3)).[pos + 3]
    in
    out
  else ""

let check_diag_down_left lst_str line pos =
  if pos >= 3 && line < List.length lst_str - 3 then
    let out =
      Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line + 1)).[pos - 1]
      ^ Char.escaped (List.nth lst_str (line + 2)).[pos - 2]
      ^ Char.escaped (List.nth lst_str (line + 3)).[pos - 3]
    in
    out
  else ""

let reverse_sam str =
  try Char.escaped str.[2] ^ Char.escaped str.[1] ^ Char.escaped str.[0]
  with Failure _ ->
    let _ = print_string "Failed reversing" in
    ""

let check_diag_right_left lst_str line pos =
  if
    pos >= 1
    && pos < String.length (List.nth lst_str line) - 1
    && line >= 1
    && line < List.length lst_str - 1
  then
    let out =
      Char.escaped (List.nth lst_str (line - 1)).[pos + 1]
      ^ Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line + 1)).[pos - 1]
    in
    if out = "SAM" then reverse_sam out else out
  else ""

let check_diag_left_right lst_str line pos =
  if
    pos >= 1
    && pos < String.length (List.nth lst_str line) - 1
    && line >= 1
    && line < List.length lst_str - 1
  then
    let out =
      Char.escaped (List.nth lst_str (line - 1)).[pos - 1]
      ^ Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line + 1)).[pos + 1]
    in
    if out = "SAM" then reverse_sam out else out
  else ""

let x_mas_checker lst_str line_number line_pos =
  if (List.nth lst_str line_number).[line_pos] = 'A' then
    if
      check_diag_left_right lst_str line_number line_pos = "MAS"
      && check_diag_right_left lst_str line_number line_pos = "MAS"
    then 1
    else 0
  else 0

let xmas_checker lst_str line_number line_pos =
  if (List.nth lst_str line_number).[line_pos] = 'X' then
    (if check_left lst_str line_number line_pos = "XMAS" then 1 else 0)
    + (if check_right lst_str line_number line_pos = "XMAS" then 1 else 0)
    + (if check_up lst_str line_number line_pos = "XMAS" then 1 else 0)
    + (if check_down lst_str line_number line_pos = "XMAS" then 1 else 0)
    + (if check_diag_up_left lst_str line_number line_pos = "XMAS" then 1 else 0)
    + (if check_diag_down_right lst_str line_number line_pos = "XMAS" then 1
       else 0)
    + (if check_diag_down_left lst_str line_number line_pos = "XMAS" then 1
       else 0)
    + if check_diag_up_right lst_str line_number line_pos = "XMAS" then 1 else 0
  else 0

let find_num_xmas condition_counter lst_str =
  let max_length = String.length (List.nth lst_str 0) in
  let max_lines = List.length lst_str in
  let rec count_occurances line_number line_pos count =
    try
      let new_count = count + condition_counter lst_str line_number line_pos in
      if line_pos + 1 >= max_length then
        if line_number + 1 >= max_lines then new_count
        else count_occurances (line_number + 1) 0 new_count
      else count_occurances line_number (line_pos + 1) new_count
    with Failure _ -> count
  in
  count_occurances 0 0 0

let () =
  let data = read_lines "data/input" in
  let count1 = find_num_xmas xmas_checker data in
  let count2 = find_num_xmas x_mas_checker data in
  Printf.printf "part 1: %d\npart 2: %d\n" count1 count2
