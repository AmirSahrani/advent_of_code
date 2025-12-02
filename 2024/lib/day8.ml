open In_channel
open Printf

[@@@ocaml.warning "-32-26"]

let readfile ic = input_lines ic

let rec print_list lst =
  match lst with
  | [] -> ()
  | hd :: tl ->
    print_char hd;
    print_list tl
;;

let to_matrix lst height width =
  Array.init_matrix height width (fun x y -> (List.nth lst x).[y])
;;

let get_frequencies_and_pos matrix =
  let height = Array.length matrix in
  let width = Array.length matrix.(0) in
  let map = Hashtbl.create height in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let current = matrix.(i).(j) in
      if current <> '.' then Hashtbl.add map current (current, (i, j))
    done
  done;
  map
;;

let project_once (fromx, fromy) (throughx, throughy) max_x max_y _ =
  let x_dist = throughx - fromx in
  let y_dist = throughy - fromy in
  let next_x = throughx + x_dist in
  let next_y = throughy + y_dist in
  if next_x >= 0 && next_y >= 0 && next_x < max_x && next_y < max_y
  then [ next_x, next_y ]
  else []
;;

let rec project_n (fromx, fromy) (throughx, throughy) max_x max_y mult =
  let x_dist = throughx - fromx in
  let y_dist = throughy - fromy in
  let next_x = throughx + (x_dist * mult) in
  let next_y = throughy + (y_dist * mult) in
  if next_x >= 0 && next_y >= 0 && next_x < max_x && next_y < max_y
  then
    (next_x, next_y)
    :: project_n (fromx, fromy) (throughx, throughy) max_x max_y (mult + 1)
  else []
;;

let rec all_pairs lst =
  match lst with
  | [] -> []
  | x :: tl -> List.map (fun y -> x, y) tl @ List.map (fun y -> y, x) tl @ all_pairs tl
;;

let check_nodes tbl max_x max_y projector =
  List.flatten
    (Hashtbl.fold
       (fun frequency _ acc ->
         let points = Hashtbl.find_all tbl frequency in
         let pairs = all_pairs points in
         let start_mult = 0 in
         let results =
           List.map
             (fun ((_, pos1), (_, pos2)) -> projector pos1 pos2 max_x max_y start_mult)
             pairs
         in
         results @ acc)
       tbl
       [])
;;

let visualize_mat mat =
  Array.iter
    (fun x ->
      print_list (Array.to_list x);
      print_endline "")
    mat
;;

let part_1 data vis =
  let array = to_matrix data (List.length data) (String.length (List.nth data 0)) in
  let max_x = Array.length array in
  let max_y = Array.length array.(0) in
  let hash = get_frequencies_and_pos array in
  let out = check_nodes hash max_x max_y project_once in
  let _ = List.iter (fun (i, j) -> Array.set (Array.get array i) j '#') out in
  if vis then visualize_mat array;
  let sum =
    Array.fold_right
      (fun arr acc ->
        Array.fold_right (fun y acc -> if y = '#' then acc + 1 else acc) arr acc)
      array
      0
  in
  printf "Part 1: %d\n" sum
;;

let part_2 data vis =
  let array = to_matrix data (List.length data) (String.length (List.nth data 0)) in
  let max_x = Array.length array in
  let max_y = Array.length array.(0) in
  let hash = get_frequencies_and_pos array in
  let out = check_nodes hash max_x max_y project_n in
  let _ = List.iter (fun (i, j) -> Array.set (Array.get array i) j '#') out in
  if vis then visualize_mat array;
  (* We can only count unique points, and since out will contain non-unique nodes,
     it is simpler to just count the unique hashtags in the visualization *)
  let sum =
    Array.fold_right
      (fun arr acc ->
        Array.fold_right (fun y acc -> if y = '#' then acc + 1 else acc) arr acc)
      array
      0
  in
  printf "Part 1: %d\n" sum
;;

let () =
  let ic = open_in "data/input" in
  let data = readfile ic in
  part_1 data false;
  part_2 data false
;;
