use std::iter::zip;

#[derive(Clone)]
enum Operator {
    Add,
    Mul,
}

struct Problem {
    operator: Operator,
    operands: Vec<u64>,
}

impl Problem {
    fn solve(&self) -> u64 {
        match self.operator {
            Operator::Add => self.operands.iter().sum(),
            Operator::Mul => self.operands.iter().product(),
        }
    }
}

struct ProblemSheet {
    problems: Vec<Problem>,
}

impl ProblemSheet {
    fn solve(&self) -> u64 {
        self.problems.iter().map(|p| p.solve()).sum()
    }
    /* fn fucked_up_parse(&self) -> ProblemSheet {
        let mut new_problems = vec![];

        for problem in &self.problems {
            let max = problem.operands.iter().max().unwrap();
            let num_digits = max.checked_ilog10().unwrap() + 1;
            let mut new_operands = vec![];
            for _ in 0..num_digits {
                new_operands.push(vec![])
            }

            for (_, num) in problem.operands.iter().enumerate() {
                let this_num_digits = num.checked_ilog10().unwrap() + 1;
                for (j, digit) in num.to_string().chars().rev().enumerate() {
                    let base: u64 = 10;
                    let power = base.pow(
                        (this_num_digits - 1)
                            .checked_sub(j as u32)
                            .expect("I should always be less than the number of digits"),
                    );
                    new_operands[j].push((digit.to_digit(10).unwrap_or(0) as u64) * power)
                }
            }
            println!("{:?}", new_operands);

            new_problems.push(Problem {
                operator: problem.operator.clone(),
                operands: new_operands.iter().map(|v| v.iter().sum::<u64>()).collect(),
            })
        }

        ProblemSheet {
            problems: new_problems,
        }
    } */
}

fn transpose<T: Clone>(input_vec: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let mut out: Vec<Vec<T>> = vec![];
    let mut tmp = 0..input_vec.first().unwrap().len();
    while let Some(_) = tmp.next() {
        out.push(vec![]);
    }
    for i in 0..input_vec.len() {
        for j in 0..input_vec.first().unwrap().len() {
            out[j].push(input_vec[i][j].clone())
        }
    }

    out
}

#[test]
fn test_transpose() {
    let input = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];
    let out_valid = vec![vec![1, 4, 7], vec![2, 5, 8], vec![3, 6, 9]];
    assert_eq!(transpose(&input), out_valid);
}

fn parse(input: &str) -> ProblemSheet {
    let mut operators = vec![];
    let mut numbers = vec![];
    for line in input.split("\n") {
        let mut row_of_numbers = vec![];
        for num in line.split_whitespace() {
            match num {
                "+" => operators.push(Operator::Add),
                "*" => operators.push(Operator::Mul),
                _ => row_of_numbers.push(num.parse::<u64>().unwrap()),
            }
        }
        if !row_of_numbers.is_empty() {
            numbers.push(row_of_numbers);
        }
    }

    numbers = transpose(&numbers);
    ProblemSheet {
        problems: zip(operators, numbers)
            .map(|(operator, operands)| Problem { operator, operands })
            .collect(),
    }
}

fn parse_2(input: &str) -> ProblemSheet {
    todo!("Part two is simple but parsing it in rust is probably gonna be very annoying");
}

pub fn solve_part1(input: &str) {
    let problem_sheet = parse(input);
    let total = problem_sheet.solve();
    print!("Cephalopods solution is {total}");
}

pub fn solve_part2(_input: &str) {

    /* let problem_sheet = parse_2(input);
    let fucked_up_sheet = problem_sheet();
    let total = fucked_up_sheet.solve();

    print!("Cephalopods solution is {total}"); */
}
