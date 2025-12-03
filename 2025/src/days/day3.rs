#[derive(Debug)]
struct Bank {
    batteries: Vec<u8>,
}

impl Bank {
    fn max_jolatage(&self, num_batteries: usize) -> u64 {
        let mut max_batteries = vec![];
        let mut highest_index = 0;
        for bat in 0..num_batteries {
            let mut highest_nth_bat = 0;
            for i in highest_index..self.batteries.len() - (num_batteries - (bat + 1)) {
                if self.batteries[i] > highest_nth_bat {
                    highest_index = i + 1;
                    highest_nth_bat = self.batteries[i];
                }
            }
            max_batteries.push(highest_nth_bat as u32);
        }
        max_batteries
            .iter()
            .rev()
            .enumerate()
            .map(|(i, &bat)| (bat as u64) * 10_u64.pow(i as u32))
            .sum()
    }
}

fn parse(input: &str) -> Vec<Bank> {
    let mut results = vec![];

    for line in input.lines() {
        let batteries = line
            .chars()
            .filter_map(|digit| digit.to_digit(10))
            .map(|d| d as u8)
            .collect();
        results.push(Bank { batteries })
    }
    results
}

pub fn solve_part1(input: &str) {
    let banks = parse(input);
    let total: u64 = banks.iter().map(|bank| bank.max_jolatage(2)).sum();
    print!("Total joltage: {total}");
}

pub fn solve_part2(input: &str) {
    let banks = parse(input);
    let total: u64 = banks.iter().map(|bank| bank.max_jolatage(12)).sum();
    print!("Total joltage: {total}");
}
