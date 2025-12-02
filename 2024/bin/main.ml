open In_channel

[@@@ocaml.warning "-32"]

let read ic = input_lines ic

let () =
  let part1, part2 = read (open_in "data/input_day17") |> Aoc.Day17.solve in
  Printf.printf "Part 1: %d\nPart 2 %d" part1 part2
;;
