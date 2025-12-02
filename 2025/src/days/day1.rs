use regex::Regex;

enum Direction {
    L,
    R,
}

struct Dial {
    value: i16,
    password: i16,
    password_x434_c49434_b: i16,
}

impl Dial {
    fn new() -> Dial {
        Dial {
            value: 50,
            password: 0,
            password_x434_c49434_b: 0,
        }
    }
    fn l(&mut self, movement: i16) {
        let to_add = if self.value != 0 { 1 } else { 0 };
        self.value = self.value - movement % 100;

        // if the value goes below zero, we must have passed the origin
        // In that case we increase the special password. If it is exactly 0
        // we can keep the dial there
        if self.value <= 0 {
            self.password_x434_c49434_b += to_add;
            if self.value != 0 {
                self.value += 100;
            }
        }
    }
    fn r(&mut self, movement: i16) {
        self.value = self.value + (movement % 100);
        if self.value >= 100 {
            self.value -= 100;
            self.password_x434_c49434_b += 1;
        }
    }
    pub fn rotate(&mut self, dir: &Direction, movement: i16) {
        self.password_x434_c49434_b += movement / 100;
        match dir {
            Direction::R => self.r(movement),
            Direction::L => self.l(movement),
        }
        if self.value == 0 {
            self.password += 1;
        }
    }
}

fn parse(input: &str) -> Vec<(Direction, i16)> {
    let pat = Regex::new(r"(R|L)(\d+)").unwrap();
    let mut results = vec![];
    for (_, [direction, movement]) in pat.captures_iter(input).map(|c| c.extract()) {
        let dir = match direction {
            "R" => Direction::R,
            "L" => Direction::L,
            _ => panic!("Invalid direction"),
        };
        results.push((dir, movement.parse::<i16>().unwrap()));
    }
    results
}
// Correct answers are test: 3, real: 1071
pub fn solve_part1(input: &str) {
    let results = parse(input);
    let mut dial = Dial::new();
    for (dir, movement) in results.iter() {
        dial.rotate(dir, *movement)
    }
    print!("Part 1: password is {}", dial.password);
}

// Correct answers are test: 6, real: 6700
pub fn solve_part2(input: &str) {
    let results = parse(input);
    let mut dial = Dial::new();
    for (dir, movement) in results.iter() {
        dial.rotate(dir, *movement)
    }
    print!("Part 2: password is {}", dial.password_x434_c49434_b);
}
