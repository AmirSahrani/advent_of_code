use regex::Regex;

fn parse(input: &str) -> Vec<(usize, usize)> {
    let pat = Regex::new(r"(\d+)-(\d+)").unwrap();
    let mut results = vec![];
    for (_, [lower, upper]) in pat.captures_iter(input).map(|c| c.extract()) {
        results.push((
            lower.parse::<usize>().unwrap(),
            upper.parse::<usize>().unwrap(),
        ));
    }
    results
}

fn invalid_num(n: &str) -> bool {
    if n.chars().count() % 2 != 0 {
        return false;
    }
    let (first, second) = n.split_at(n.len() / 2);
    if first != second {
        return false;
    }
    return true;
}

fn divisors(n: usize) -> Vec<usize> {
    (2..n + 1).filter(|i| n % i == 0).collect()
}

fn invalid_repeated_num(n: &str) -> bool {
    let len_n = n.chars().count();
    for subs in divisors(len_n) {
        // sub_strings.clear();
        let size = len_n / subs;
        let sub_strings = n
            .as_bytes()
            .chunks(size)
            .map(|buf| unsafe { str::from_utf8_unchecked(buf) })
            .collect::<Vec<&str>>();

        let mut all_repeating = true;
        for i in 0..sub_strings.len() - 1 {
            let elem: &str = sub_strings[i];
            let elem_next: &str = sub_strings[i + 1];
            if elem != elem_next {
                all_repeating = false;
            }
        }
        if all_repeating {
            /* println!(
                "{n} is invalid, because {0}, is repeating {subs} times",
                sub_strings.first().unwrap()
            ); */
            return true;
        }
    }
    return false;
}

fn find_invalid(lower: usize, upper: usize, disallow_repeat: bool) -> Vec<u64> {
    let mut invalid = vec![];
    for num in lower..upper + 1 {
        let str_of_num = num.to_string();

        if disallow_repeat {
            if invalid_num(&str_of_num) {
                invalid.push(num as u64)
            }
        } else {
            if invalid_repeated_num(&str_of_num) {
                invalid.push(num as u64)
            }
        }
    }
    invalid
}
pub fn solve_part1(input: &str) {
    let ranges = parse(input);
    let invalid_ids = ranges
        .iter()
        .map(|(lower, upper)| find_invalid(*lower, *upper, true));

    let sum = invalid_ids.fold(0, |acc_outer, subarr| {
        acc_outer
            + subarr
                .into_iter()
                .fold(0, |acc_inner, elem| acc_inner + elem)
    });

    print!("Got a sum of {sum}");
}

// takes 5 seconds in the current state
pub fn solve_part2(input: &str) {
    let ranges = parse(input);
    let invalid_ids = ranges
        .iter()
        .map(|(lower, upper)| find_invalid(*lower, *upper, false));

    let sum = invalid_ids.fold(0, |acc_outer, subarr| {
        acc_outer
            + subarr
                .into_iter()
                .fold(0, |acc_inner, elem| acc_inner + elem)
    });

    print!("Got a sum of {sum}");
}
