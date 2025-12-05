use regex::Regex;

type Id = u64;
type Ids = Vec<Id>;
type Ranges = Vec<(Id, Id)>;

fn parse_brute(input: &str) -> (Ranges, Ids) {
    let (fresh, produce) = input.split_once("\n\n").unwrap();
    let mut ranges = vec![];
    let mut ids = vec![];
    let re = Regex::new(r"(\d+)-(\d+)").unwrap();
    for (_, [lower, upper]) in re.captures_iter(fresh).map(|c| c.extract()) {
        ranges.push((lower.parse::<u64>().unwrap(), upper.parse::<u64>().unwrap()));
    }

    for line in produce.split_whitespace() {
        ids.push(line.parse::<u64>().unwrap())
    }

    (ranges, ids)
}

fn is_id_fresh(id: Id, ranges: &Ranges) -> bool {
    for (lower, upper) in ranges {
        if (id <= *upper) && (*lower <= id) {
            return true;
        }
    }
    false
}

fn merge_ranges(ranges: &Ranges) -> Ranges {
    let mut new_ranges = vec![];
    for &(lower, upper) in ranges.iter() {
        let mut caught = false;
        for (i, &(l, u)) in new_ranges.iter().enumerate() {
            if lower >= l && lower <= u {
                // range fits entirely in another
                if upper >= l && upper <= u {
                    caught = true;
                    break;
                }
                // only lower part first
                else {
                    new_ranges[i] = (l, upper.clone());
                    caught = true;
                    break;
                }
            } else if upper >= l && upper <= u {
                // only upper part first
                if lower <= l {
                    new_ranges[i] = (lower.clone(), (u).clone());
                    caught = true;
                    break;
                }
            }
        }
        if !caught {
            new_ranges.push((lower.clone(), upper.clone()))
        }
    }
    new_ranges
}
fn total_merge(ranges: &mut Ranges) -> Ranges {
    ranges.sort_by_key(|x| x.0);
    let mut new_ranges = ranges.clone();
    let mut prev = new_ranges.len();
    loop {
        new_ranges = merge_ranges(&new_ranges);
        if new_ranges.len() == prev {
            break;
        }
        prev = new_ranges.len()
    }
    new_ranges
}

fn len_range(lower: &Id, upper: &Id) -> u64 {
    // inclusive of upper
    (*upper + 1) - *lower
}

pub fn solve_part1(input: &str) {
    let (fresh_ranges, ids) = parse_brute(input);
    let fresh = ids
        .iter()
        .filter(|id| is_id_fresh(**id, &fresh_ranges))
        .count();
    print!("Found {fresh} fresh products");
}

// original solution took 6705216 ns, 357907198933892
pub fn solve_part2(input: &str) {
    let (mut fresh_ranges, _ids) = parse_brute(input);
    let merged_ranges: Ranges = total_merge(&mut fresh_ranges);

    let total_fresh_ids: u64 = merged_ranges
        .iter()
        .map(|(lower, upper)| len_range(lower, upper))
        .sum();
    print!("Found: {total_fresh_ids} fresh ids");
}
