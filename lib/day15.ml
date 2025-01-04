type move =
  | Up
  | Down
  | Left
  | Right

type tile =
  | Empty
  | Wall
  | Box
  | Robot

let parse_next_move m =
  match m with
  | '^' -> Up
  | 'v' -> Down
  | '>' -> Right
  | '<' -> Left
  | _ -> failwith "finished moving"
;;

let parse_tile m =
  match m with
  | '.' -> Empty
  | '#' -> Wall
  | 'O' -> Box
  | '@' -> Robot
  | _ -> failwith "Invalid tile"
;;

let char_of_tile m =
  match m with
  | Empty -> '.'
  | Wall -> '#'
  | Box -> 'O'
  | Robot -> '@'
;;

let split str =
  let rec aux s out =
    if String.length s = 0
    then out
    else aux (String.sub s 1 (String.length s - 1)) (s.[0] :: out)
  in
  aux str [] |> List.rev
;;

let print_mat mat =
  Array.iter
    (fun row ->
      Array.iter (fun elem -> print_char (elem |> char_of_tile)) row;
      print_newline ())
    mat
;;

let parse_data input =
  let rec aux inp mp moves =
    match inp with
    | [] -> mp |> List.rev, moves |> List.rev
    | hd :: tl ->
      if String.contains hd '#' then aux tl (hd :: mp) moves else aux tl mp (hd :: moves)
  in
  let map_out, moves_out = aux input [] [] in
  let moves_final = String.concat "" moves_out in
  let map_final =
    Array.of_list
      (List.map (fun x -> split x |> List.map parse_tile |> Array.of_list) map_out)
  in
  map_final, moves_final
;;

let locate_robot grid =
  let i = Array.find_index (fun x -> Array.exists (fun y -> y = Robot) x) grid in
  let idx =
    match i with
    | Some idx -> idx
    | _ -> failwith "No robot"
  in
  let j = Array.find_index (fun x -> x = Robot) grid.(idx) in
  match j with
  | Some jdx -> idx, jdx
  | _ -> failwith "No robot"
;;

let find_empty_tile (di, dj) (i, j) grid =
  let rec aux ni nj =
    if grid.(ni).(nj) = Wall
    then None
    else if grid.(ni).(nj) = Empty
    then Some (ni, nj)
    else aux (ni + di) (nj + dj)
  in
  aux i j
;;

let pull_into (i, j) (ri, rj) grid =
  let rec aux ni nj =
    if grid.(ni).(nj) = Robot
    then (
      grid.(ni).(nj) <- Empty;
      grid)
    else (
      grid.(ni).(nj) <- grid.(ni + ri).(nj + rj);
      aux (ni + ri) (nj + rj))
  in
  aux i j
;;

let attempt_move (di, dj) (i, j) grid =
  let empty_tile = find_empty_tile (di, dj) (i, j) grid in
  match empty_tile with
  | None -> (i, j), grid
  | Some (ni, nj) -> (i + di, j + dj), pull_into (ni, nj) (-di, -dj) grid
;;

let step loc map m =
  let new_loc, new_map =
    match m with
    | Up -> attempt_move (-1, 0) loc map
    | Down -> attempt_move (1, 0) loc map
    | Left -> attempt_move (0, -1) loc map
    | Right -> attempt_move (0, 1) loc map
  in
  new_loc, new_map
;;

let sum_gps mat =
  let sum = ref 0 in
  Array.iteri
    (fun i row ->
      Array.iteri (fun j tile -> if tile = Box then sum := !sum + (100 * i) + j) row)
    mat;
  !sum
;;

let part1 data =
  let grid, moves = parse_data data in
  let robot_loc = locate_robot grid in
  let rec aux loc grid m =
    if String.length m = 0
    then grid
    else (
      let curr = parse_next_move m.[0] in
      let next = String.sub m 1 (String.length m - 1) in
      let loc, grid = step loc grid curr in
      aux loc grid next)
  in
  let out = aux robot_loc grid moves in
  sum_gps out
;;

let solve data = part1 data, 0
