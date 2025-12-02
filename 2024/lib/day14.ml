open In_channel
open Printf

[@@@ocaml.warning "-32"]

type robot = { pos : int * int; vel : int * int }

let read ic = input_lines ic

let create_robot lst =
  match lst with
  | [ px; py; vx; vy ] -> { pos = (px, py); vel = (vx, vy) }
  | _ -> failwith "failed to create robot"

let print_robot robot =
  let px, py = robot.pos in
  let vx, vy = robot.vel in
  printf "Robot position: %d, %d velocity: %d, %d\n" px py vx vy

let update_pos robot max_x max_y =
  let x, y = robot.pos in
  let vx, vy = robot.vel in
  let nx = (x + vx + max_x) mod max_x in
  let ny = (y + vy + max_y) mod max_y in
  let new_robot = { pos = (nx, ny); vel = (vx, vy) } in
  new_robot

let parse_robot input =
  let digit_regex = Re.Perl.compile_pat "-?\\d+" in
  let numbers = Re.matches digit_regex input in
  List.map int_of_string numbers |> create_robot

let display robots max_x max_y =
  (* Unix.sleepf (1.0 /. 30.0); *)
  let mat = Array.make_matrix max_x max_y false in
  List.iter
    (fun r ->
      let x, y = r.pos in
      mat.(x).(y) <- true)
    robots;

  if
    Array.exists
      (fun row ->
        Array.exists
          (fun i ->
            i <= Array.length row - 8
            && row.(i)
            && row.(i + 1)
            && row.(i + 2)
            && row.(i + 3)
            && row.(i + 4)
            && row.(i + 5)
            && row.(i + 6)
            && row.(i + 7))
          (Array.init (Array.length row) Fun.id))
      mat
  then (
    Array.iter
      (fun row ->
        Array.iter
          (fun entry -> if entry then print_string "#" else print_string " ")
          row;
        print_endline "")
      mat;
    true)
  else false

let get_robots lst = List.map parse_robot lst

let rec simulate robots grid_x grid_y iter =
  match iter with
  | 0 -> robots
  | _ ->
      printf "Iteration: %d--------------------------------------------\n\n"
        (10000 - iter);
      if display robots grid_x grid_y then robots
      else
        simulate
          (List.map (fun x -> update_pos x grid_x grid_y) robots)
          grid_x grid_y (iter - 1)

let locate_robot robot max_x max_y =
  let x, y = robot.pos in
  let mid_x = max_x / 2 in
  let mid_y = max_y / 2 in
  if x < mid_x && y < mid_y then (1, 0, 0, 0)
  else if x >= mid_x + 1 && y < mid_y then (0, 1, 0, 0)
  else if x < mid_x && y >= mid_y + 1 then (0, 0, 1, 0)
  else if x >= mid_x + 1 && y >= mid_y + 1 then (0, 0, 0, 1)
  else (0, 0, 0, 0)

let part_1 data grid_x grid_y iter =
  let robots = get_robots data in
  let final_robots = simulate robots grid_x grid_y iter in
  let locations =
    List.map (fun x -> locate_robot x grid_x grid_y) final_robots
  in
  let quadrands =
    List.fold_left
      (fun (sum1, sum2, sum3, sum4) (x1, x2, x3, x4) ->
        (sum1 + x1, sum2 + x2, sum3 + x3, sum4 + x4))
      (0, 0, 0, 0) locations
  in
  let tl, tr, bl, br = quadrands in
  printf "%d, %d, %d, %d\n" tl tr bl br;
  tl * tr * bl * br

let () =
  let data = open_in "data/input" |> read in
  let out = part_1 data 101 103 10000 in
  print_int out
