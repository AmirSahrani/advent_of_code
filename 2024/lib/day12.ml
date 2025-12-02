open In_channel
open Printf
(* open Set *)

[@@@ocaml.warning "-32"]

let readdata ic = input_lines ic

let rec split str =
  let len = String.length str in
  if len = 0 then [] else str.[0] :: split (String.sub str 1 (len - 1))

module TupleSet = Set.Make (struct
  type t = int * int

  let compare (a1, a2) (b1, b2) =
    if a1 = b1 then compare a2 b2 else compare a1 b1
end)

let get_neighbors mat i j =
  let directions = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
  List.map
    (fun (x, y) ->
      let new_i = i + x in
      let new_j = j + y in
      let c = try mat.(new_i).(new_j) with _ -> '~' in
      (c, new_i, new_j))
    directions

let rec explore mat i j target already_included =
  if TupleSet.mem (i, j) !already_included then [ (0, 0) ]
  else (
    already_included := TupleSet.add (i, j) !already_included;
    let neighbors = get_neighbors mat i j in
    List.flatten
      (List.map
         (fun (x, new_i, new_j) ->
           if x = target then
             if not (TupleSet.mem (new_i, new_j) !already_included) then
               (1, 0) :: explore mat new_i new_j target already_included
             else [ (0, 0) ]
           else [ (0, 1) ])
         neighbors))

let print_tuple tup =
  match tup with
  | i, j, di, dj, char ->
      printf "%d, %d, %d, %d, %c," i j di dj (char_of_int char)

let rec explore2 mat i j target already_included fences =
  if TupleSet.mem (i, j) !already_included then [ (0, 0) ]
  else (
    already_included := TupleSet.add (i, j) !already_included;
    let neighbors = get_neighbors mat i j in
    List.flatten
      (List.map
         (fun (x, new_i, new_j) ->
           if TupleSet.mem (new_i, new_j) !already_included && x = target then
             [ (0, 0) ]
           else if x = target then
             (1, 0) :: explore2 mat new_i new_j target already_included fences
           else
             let di, dj = (i - new_i, j - new_j) in
             let exist_fence_block =
               List.exists
                 (fun (i', j', di', dj', t) ->
                   (i' + dj', j' + di', di', dj', t)
                   = (new_i, new_j, di, dj, target)
                   || (i' - dj', j' - di', di', dj', t)
                      = (new_i, new_j, di, dj, target))
                 !fences
             in
             List.iter
               (fun x ->
                 if not (List.mem x !fences) then fences := x :: !fences)
               [
                 (new_i, new_j, di, dj, target);
                 (* removing on of these fixes the c issue, but only in one direction? check up and down as well *)
                 (* (new_i + dj, new_j + di, di, dj, target); *)
                 (* (new_i - dj, new_j - di, di, dj, target); *)
               ];
             if exist_fence_block then [ (0, 0) ] else [ (0, 1) ])
         neighbors))

let part_1 matrix =
  let counts = Hashtbl.create 10 in
  let already_included = ref TupleSet.empty in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j entry ->
          if TupleSet.mem (i, j) !already_included then ()
          else
            let out = (1, 0) :: explore matrix i j entry already_included in
            let plots, border =
              List.fold_right
                (fun (p, b) (acc_p, acc_b) -> (acc_p + p, acc_b + b))
                out (0, 0)
            in
            if Hashtbl.mem counts entry then (
              let c = Hashtbl.find counts entry in
              Hashtbl.remove counts entry;
              Hashtbl.add counts entry (c + (plots * border)))
            else Hashtbl.add counts entry (plots * border))
        row)
    matrix;
  Hashtbl.fold (fun _ count acc -> acc + count) counts 0

let part_2 matrix =
  let counts = Hashtbl.create 10 in
  let already_included = ref TupleSet.empty in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j entry ->
          if TupleSet.mem (i, j) !already_included then ()
          else
            let out =
              let fences = ref [] in
              (1, 0) :: explore2 matrix i j entry already_included fences
            in
            let plots, border =
              List.fold_right
                (fun (p, b) (acc_p, acc_b) -> (acc_p + p, acc_b + b))
                out (0, 0)
            in
            printf "%c; %d %d \n" entry plots border;
            if Hashtbl.mem counts entry then (
              let c = Hashtbl.find counts entry in
              Hashtbl.remove counts entry;
              Hashtbl.add counts entry (c + (plots * border)))
            else Hashtbl.add counts entry (plots * border))
        row)
    matrix;
  Hashtbl.fold (fun _ count acc -> acc + count) counts 0

let () =
  let data = readdata (open_in "data/test") in
  let sdata = List.map split data in
  let arrays = List.map Array.of_list sdata in
  let mat = Array.of_list arrays in
  let perimiter = part_1 mat in
  let perimiter_discount = part_2 mat in
  printf "Part 1: %d\n" perimiter;
  printf "Part 2: %d\n" perimiter_discount
