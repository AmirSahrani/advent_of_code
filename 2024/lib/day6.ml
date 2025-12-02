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

module TupleSet = Set.Make (struct
  type t = int * int

  let compare (a1, a2) (b1, b2) =
    match compare a1 b1 with 0 -> compare a2 b2 | c -> c
end)

let countDotsInString str =
  let count = ref 0 in
  String.iter (fun x -> if x = '.' then incr count) str;
  !count

let countdots lst_str =
  let counts = List.map countDotsInString lst_str in
  List.fold_right ( + ) counts 0

let replace str c n = String.mapi (fun i x -> if i = n then c else x) str

let place_obstace lst_str indx pos =
  List.mapi
    (fun i x -> if i = indx then replace (List.nth lst_str i) '#' pos else x)
    lst_str

let next_position lst_str indx pos (vertical, horizontal) =
  (List.nth lst_str (indx + vertical)).[pos + horizontal]

let turn_90_degrees (horizontal, vertical) =
  match (horizontal, vertical) with
  | -1, 0 -> (0, 1)
  | 0, 1 -> (1, 0)
  | 1, 0 -> (0, -1)
  | 0, -1 -> (-1, 0)
  | _ -> failwith "invalid input"

let patrol_guard lst_str start_indx start_pos =
  let starting_direction = (-1, 0) in
  let max_row = List.length lst_str in
  let max_dots = countdots lst_str in
  let rec move indx pos (x, y) history count =
    try
      if count >= max_dots then failwith "infinite loop";
      (* Check grid boundaries *)
      if indx = 0 || indx = max_row - 1 then failwith "Guard left grid";
      let row = List.nth lst_str indx in
      (* Printf.printf "max: %d pos: %d\n" (String.length row) pos; *)
      if pos = 0 || pos = String.length row - 1 then failwith "Guard left grid";

      (* Get the next tile and update history *)
      (* Printf.printf "%d %d\n" (indx + 1) (pos + 1); *)
      let next_tile = next_position lst_str indx pos (x, y) in
      let history = TupleSet.add (indx, pos) history in

      (* Decide the next move based on tile type *)
      match next_tile with
      | '#' -> move indx pos (turn_90_degrees (x, y)) history (count + 1)
      | _ -> move (indx + x) (pos + y) (x, y) history (count + 1)
    with Failure x -> (x, history)
  in
  move start_indx start_pos starting_direction TupleSet.empty 0

let find_start lststr =
  let indx = List.find_index (fun x -> String.contains x '^') lststr in
  match indx with
  | None -> failwith "not found"
  | Some x -> (x, String.index (List.nth lststr x) '^')

let containsLoop lststr indx pos =
  let status, _ = patrol_guard lststr indx pos in
  match status with "infinite loop" -> true | _ -> false

let all_starts max_x max_y =
  List.init (max_x + 1) (fun x -> List.init (max_y + 1) (fun y -> (x, y)))
  |> List.flatten

let () =
  let data = read_lines "data/input" in
  let start_x, start_y = find_start data in
  let max_x = List.length data in
  let max_y = String.length (List.nth data 0) in
  let potential_obstacle_pos = all_starts max_x max_y in
  let potential_obstacle_pos =
    List.filter
      (fun (row, col) -> if row = start_x && col = start_y then false else true)
      potential_obstacle_pos
  in
  (* List.iter
     (fun (x, y) -> Printf.printf "(%d, %d)\n" x y)
     potential_obstacle_pos; *)
  let loops =
    List.map
      (fun (row, col) ->
        containsLoop (place_obstace data row col) start_x start_y)
      potential_obstacle_pos
  in
  let contains_loops = List.filter (fun x -> x) loops in
  let _, history = patrol_guard data start_x start_y in
  print_string "Part 1: ";
  print_int (TupleSet.cardinal history + 1);
  print_string "\n";
  print_string "Part 2: ";
  print_int (List.length contains_loops);
  print_string "\n"
