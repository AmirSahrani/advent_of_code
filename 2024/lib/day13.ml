open In_channel
open Printf

let mat_prod mat arr =
  ( Array.fold_right
      (fun x acc -> acc +. x)
      (Array.map2 (fun x y -> x *. y) mat.(0) arr)
      0.,
    Array.fold_right
      (fun x acc -> acc +. x)
      (Array.map2 (fun x y -> x *. y) mat.(1) arr)
      0. )

let local_search num_a num_b a_x a_y b_x b_y t_x t_y =
  let result = ref None in
  for i = num_a - 10 to num_a + 10 do
    for j = num_b - 10 to num_b + 10 do
      let x' = (i * int_of_float a_x) + (j * int_of_float b_x) in
      let y' = (i * int_of_float a_y) + (j * int_of_float b_y) in
      if x' = int_of_float t_x && y' = int_of_float t_y then
        result := Some ((3 * i) + j)
    done
  done;
  !result

let solve inp =
  match inp with
  | [ a_x; b_x; a_y; b_y; t_x; t_y ] ->
      let t_x = t_x +. 10000000000000. in
      let t_y = t_y +. 10000000000000. in
      let det = (a_x *. b_y) -. (b_x *. a_y) in
      if det = 0.0 then None
      else
        let inv = [| [| b_y; -.a_y |]; [| -.b_x; a_x |] |] in
        Array.iter (fun row -> Array.map_inplace (fun x -> x /. det) row) inv;
        let press_a, press_b = mat_prod inv (Array.of_list [ t_x; t_y ]) in
        (* if press_a > 100. || press_a < 0. || press_b < 0. || press_b > 100. then
          None *)
        if press_a < 0. || press_b < 0. then None
        else
          let pa = int_of_float press_a in
          let pb = int_of_float press_b in
          local_search pa pb a_x b_x a_y b_y t_x t_y
  | _ -> None

let readdata ic = input_lines ic |> String.concat "\n"

let parse text =
  let regex =
    Re.Perl.compile_pat
      "Button A: X\\+\\d+, Y\\+\\d+\n\
       Button B: X\\+\\d+, Y\\+\\d+\n\
       Prize: X=\\d+, Y=\\d+"
  in
  let digit_regex = Re.Perl.compile_pat "\\d+" in
  let blocks = Re.all regex text in
  List.map
    (fun block ->
      List.map float_of_string (Re.matches digit_regex (Re.Group.get block 0)))
    blocks

let part_1 data =
  let parsed = parse data in
  let sols = List.map (fun x -> solve x) parsed in
  List.fold_right
    (fun x acc -> match x with None -> acc | Some y -> acc + y)
    sols 0

let () =
  let data = readdata (open_in "data/input") in
  let solution = part_1 data in
  printf "Part 2: %d\n" solution
