open Int64

let find register label = to_int @@ Hashtbl.find register label
let set register label value = Hashtbl.replace register label (of_int value)

let combo register op =
  if op >= 0 && op < 4
  then op
  else if op = 4
  then find register "A"
  else if op = 5
  then find register "B"
  else if op = 6
  then find register "C"
  else failwith "invalid program"
;;

let adv register op pointer =
  let _ =
    find register "A" / (int_of_float @@ Float.pow 2.0 (float_of_int (combo register op)))
    |> set register "A"
  in
  None, pointer + 2
;;

let bxl register op pointer =
  let left = find register "B" in
  let right = op in
  let _ = left lxor right |> set register "B" in
  None, pointer + 2
;;

let bst register op pointer =
  let _ = combo register op mod 8 |> set register "B" in
  None, pointer + 2
;;

let jnz register op pointer =
  if find register "A" = 0 then None, pointer + 2 else None, op
;;

let bxc register _ pointer =
  let left = find register "B" in
  let right = find register "C" in
  let _ = left lxor right |> set register "B" in
  None, pointer + 2
;;

let out register op pointer =
  let output = combo register op mod 8 in
  Some output, pointer + 2
;;

let bdv register op pointer =
  let _ =
    find register "A" / (int_of_float @@ Float.pow 2.0 (float_of_int (combo register op)))
    |> set register "B"
  in
  None, pointer + 2
;;

let cdv register op pointer =
  let _ =
    find register "A" / (int_of_float @@ Float.pow 2.0 (float_of_int (combo register op)))
    |> set register "C"
  in
  None, pointer + 2
;;

let operand_to_fun = function
  | 0 -> adv
  | 1 -> bxl
  | 2 -> bst
  | 3 -> jnz
  | 4 -> bxc
  | 5 -> out
  | 6 -> bdv
  | 7 -> cdv
  | _ -> failwith "halt"
;;

let fix_out o = o |> List.filter_map (fun x -> x) |> List.rev

let run_program a b c program =
  let registers = Hashtbl.create 3 in
  Hashtbl.add registers "A" a;
  Hashtbl.add registers "B" b;
  Hashtbl.add registers "C" c;
  let inst_pointer = 0 in
  let program_output = ref [] in
  let rec aux pointer =
    if pointer > List.length program
    then ()
    else (
      try
        let operand = List.nth program pointer |> operand_to_fun in
        let o, p = operand registers (List.nth program (pointer + 1)) pointer in
        program_output := o :: !program_output;
        aux p
      with
      | Failure _ -> ())
  in
  aux inst_pointer;
  find registers "A", find registers "B", find registers "C", !program_output |> fix_out
;;

let part1 _ =
  let program = [ 2; 4; 1; 1; 7; 5; 4; 4; 1; 4; 0; 3; 5; 5; 3; 0 ] in
  let _, _, _, output = run_program 46337277L 0L 0L program in
  output |> List.map string_of_int |> String.concat "," |> print_endline;
  0
;;

(* pretty much stolen from: https://github.com/veeenu/adventofcode2024/blob/main/day17.ml *)
let reverse_interpret program =
  let rec rev_interp_step a b c count =
    let candidates =
      [ 0L; 1L; 2L; 3L; 4L; 5L; 6L; 7L ]
      |> List.map (fun offset -> offset, run_program (add a offset) b c program)
      |> List.filter (fun (_, (_, _, _, result)) ->
        List.nth program count = List.hd result)
    in
    if count = 0
    then candidates |> List.map (fun (offset, _) -> add a offset)
    else
      candidates
      |> List.map (fun (offset, (_, b, c, _)) ->
        rev_interp_step (shift_left (add a offset) 3) (of_int b) (of_int c) (count - 1))
      |> List.flatten
  in
  rev_interp_step 0L 0L 0L (List.length program - 1)
;;

let part2 _ =
  let program = [ 0; 3; 5; 4; 3; 0 ] in
  let output = reverse_interpret program in
  List.fold_left min max_int output |> to_int |> print_int;
  print_endline "\n";
  0
;;

let solve data = part1 data, part2 data
