open Day15

type tile =
  | Wall
  | Tile
  | Target
  | Source

let parse_t t =
  match t with
  | '#' -> Wall
  | '.' -> Tile
  | 'E' -> Target
  | 'S' -> Source
  | _ -> failwith "Invalid tile"
;;

let parse data =
  Array.of_list (List.map (fun x -> split x |> List.map parse_t |> Array.of_list) data)
;;

let dir_of_pi p = int_of_float @@ -.sin p, int_of_float @@ cos p

let weight_matrix_like mat =
  let dirs = 4 in
  let rows = Array.length mat in
  let cols = Array.length mat.(0) in
  let mat_out =
    Array.init rows (fun _ -> Array.init_matrix cols dirs (fun _ _ -> infinity))
  in
  mat_out
;;

let locate t grid =
  let i = Array.find_index (fun x -> Array.exists (fun y -> y = t) x) grid in
  let idx =
    match i with
    | Some idx -> idx
    | _ -> failwith "Not found"
  in
  let j = Array.find_index (fun x -> x = t) grid.(idx) in
  match j with
  | Some jdx -> idx, jdx
  | _ -> failwith "Not found"
;;

module PrioritySet = Set.Make (struct
    type t = int * int * float * float

    let compare (_, _, _, a) (_, _, _, b) = if a < b then -1 else if a > b then 1 else 0
  end)

let print_coords i j = Printf.printf "i: %d j: %d\n" (i + 1) (j + 1)

let get_neighbors mat (i, j) dir =
  let turns = [ -.Float.pi /. 2.0; 0.0; Float.pi /. 2.0 ] in
  let penalties = [ 1000.0; 1.0; 1000.0 ] in
  List.filter_map
    (fun (turn, penalty) ->
      let new_dir = dir +. turn in
      let di, dj = dir_of_pi new_dir in
      let ni = i + di in
      let nj = j + dj in
      try
        match mat.(ni).(nj) with
        | Wall -> None
        | _ -> Some (ni, nj, new_dir, penalty)
      with
      | Invalid_argument _ -> None)
    (List.combine turns penalties)
;;

let direction_dim_fom dir =
  let d = dir_of_pi dir in
  match d with
  | 1, 0 -> 0
  | -1, 0 -> 1
  | 0, 1 -> 2
  | 0, -1 -> 3
  | j, k ->
    print_coords j k;
    failwith "Wrong Direction"
;;

let clamp dir =
  let quarter_pi = Float.pi /. 2.0 in
  let nearest_quarter = Float.round (dir /. quarter_pi) in
  nearest_quarter *. quarter_pi
;;

let cheapest_path mat =
  let weight_mat = weight_matrix_like mat in
  let si, sj = locate Source mat in
  let ti, tj = locate Target mat in
  print_coords ti tj;
  (* Initialize priority queue with starting position *)
  let pq = ref PrioritySet.empty in
  pq := PrioritySet.add (si, sj, 0.0, 0.0) !pq;
  let rec aux () =
    match PrioritySet.min_elt_opt !pq with
    | None -> -1. (* No path found *)
    | Some (i, j, dir, cost) ->
      pq := PrioritySet.remove (i, j, dir, cost) !pq;
      if (i, j) = (ti, tj)
      then cost
      else (
        let neighbors = get_neighbors mat (i, j) dir in
        List.iter
          (fun (ni, nj, new_dir, step_cost) ->
            let dir_idx = direction_dim_fom new_dir in
            let new_cost = cost +. step_cost in
            if new_cost < weight_mat.(ni).(nj).(dir_idx)
            then (
              weight_mat.(ni).(nj).(dir_idx) <- new_cost;
              pq := PrioritySet.add (ni, nj, clamp new_dir, new_cost) !pq))
          neighbors;
        aux ())
  in
  aux ()
;;

let part1 data = data |> parse |> cheapest_path |> int_of_float
let part2 _ = 0
let solve data = part1 data, part2 0
