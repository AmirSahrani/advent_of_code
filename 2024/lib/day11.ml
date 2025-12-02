open Printf

[@@@ocaml.warning "-32"]

let even_length num = String.length (string_of_int num) mod 2 = 0

let chunk_list lst chunk_size =
  let rec aux acc current n = function
    | [] -> current :: acc
    | x :: xs ->
        if n = chunk_size then aux (current :: acc) [ x ] 1 xs
        else aux acc (x :: current) (n + 1) xs
  in
  aux [] [] 0 lst

let split_even num =
  let str_num = string_of_int num in
  let len = String.length str_num in
  List.map int_of_string
    [ String.sub str_num 0 (len / 2); String.sub str_num (len / 2) (len / 2) ]

let blink_elem elem =
  if elem = 0 then [ 1 ]
  else if even_length elem then split_even elem
  else [ elem * 2024 ]

let rec blink lst =
  match lst with [] -> [] | hd :: tl -> blink_elem hd @ blink tl

let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g

let rec apply_blink lst n =
  if n = 0 then lst else apply_blink (blink lst) (n - 1)

let rec apply_memo_blink hsh n =
  if n = 0 then hsh
  else
    let hsh_cpy = Hashtbl.create 10 in
    Hashtbl.iter
      (fun stone count ->
        let out = blink_elem stone in
        List.iter
          (fun blinked_stone ->
            let r = Hashtbl.find_opt hsh_cpy blinked_stone in
            match r with
            | None -> Hashtbl.add hsh_cpy blinked_stone count
            | Some result ->
                Hashtbl.replace hsh_cpy blinked_stone (result + count))
          out)
      hsh;
    apply_memo_blink hsh_cpy (n - 1)

let time f x =
  let t1 = Mtime_clock.elapsed () in
  let fx = f x in
  let t2 = Mtime_clock.elapsed () in
  let duration_ns = Mtime.Span.to_uint64_ns (Mtime.Span.abs_diff t1 t2) in
  Printf.printf "Execution time: %Ld ns\n" duration_ns;
  fx

let h_from_list lst =
  let h = Hashtbl.create (List.length lst) in
  List.iter
    (fun x ->
      let r = Hashtbl.find_opt h x in
      match r with
      | None -> Hashtbl.add h x 1
      | Some result -> Hashtbl.replace h x (result + 1))
    lst;
  h

let () =
  let _ = [ 125; 17 ] in
  (*test input*)
  let lst = [ 554735; 45401; 8434; 0; 188; 7487525; 77; 7 ] in
  let h = h_from_list lst in
  let blinked = time apply_memo_blink h 25 in
  let blinked2 = time apply_memo_blink h 75 in
  printf "\nPart 1: %d, Dict size: %d\n"
    (Hashtbl.fold (fun _ count acc -> acc + count) blinked 0)
    (Hashtbl.length blinked);
  printf "\nPart 2: %d, Dict size: %d\n"
    (Hashtbl.fold (fun _ count acc -> acc + count) blinked2 0)
    (Hashtbl.length blinked2)
