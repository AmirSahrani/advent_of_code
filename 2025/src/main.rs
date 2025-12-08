mod days;
mod utils;

use days::day8 as day;

fn main() {
    let input_file = "inputs/input-8.txt";
    let test_input_file = "inputs/test_input-8.txt";
    let input = utils::read_file(input_file);
    let test_input = utils::read_file(test_input_file);
    println!("Test inputs:");
    let time = utils::benchmark(day::solve_part1, &test_input);
    println!(", found in {:.2?} ns", time.as_nanos());
    let time = utils::benchmark(day::solve_part2, &test_input);
    println!(", found in {:.2?} ns", time.as_nanos());

    /* println!("\nReal inputs:");
    let time = utils::benchmark(day::solve_part1, &input);
    println!(", found in {:.2?} ns", time.as_nanos());
    let time = utils::benchmark(day::solve_part2, &input);
    println!(", found in {:.2?} ns", time.as_nanos()); */
}
