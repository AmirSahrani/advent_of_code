use std::collections::{HashMap, VecDeque};

use std::fmt;

const MAX_BUTTONS: usize = 32;

#[derive(Debug)]
struct Machine {
    state: u32,
    buttons: [u32; MAX_BUTTONS],
    target_state: u32,
    _joltage: [u32; MAX_BUTTONS],
}
impl fmt::Display for Machine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:b} -> {:b}, Buttons: {:?}",
            self.state, self.target_state, self.buttons
        )
    }
}

impl Machine {
    fn new(
        &self,
        buttons: [u32; MAX_BUTTONS],
        target_state: u32,
        joltage: [u32; MAX_BUTTONS],
    ) -> Machine {
        Machine {
            state: 0,
            buttons,
            target_state,
            _joltage: joltage,
        }
    }
    fn is_solved(&self) -> bool {
        self.state == self.target_state
    }

    fn min_steps_to_solve(&mut self) -> Option<usize> {
        if self.is_solved() {
            return Some(0);
        }

        let mut queue = VecDeque::new();
        let mut visited = HashMap::new();

        queue.push_back((self.state, 0));
        visited.insert(self.state, 0);

        while let Some((current_state, steps)) = queue.pop_front() {
            self.state = current_state;
            for &button_mask in self.buttons.iter() {
                if button_mask == 0 {
                    continue;
                }

                let new_state = current_state ^ (button_mask);

                if new_state == self.target_state {
                    return Some(steps + 1);
                }
                if !visited.contains_key(&new_state) {
                    visited.insert(new_state, steps + 1);
                    queue.push_back((new_state, steps + 1));
                }
            }
        }
        None
    }
}
fn parse(input: &str) -> Vec<Machine> {
    let mut machines = vec![];

    // line is of format [.|#+] (\d,\d, ...)+ {\d, \d,...}
    for line in input.lines() {
        let mut state_input = false;
        let mut button_input = false;
        let mut joltage_input = false;
        let mut idx = 0;
        let mut buttons = [0u32; MAX_BUTTONS];
        let mut state = 0;
        let mut button = 0;
        let mut button_count = 0;
        let mut joltage = [0u32; MAX_BUTTONS];
        let mut joltage_count = 0;
        for c in line.chars() {
            match c {
                '[' => {
                    state_input = true;
                    idx = 0;
                    continue;
                }
                ']' => {
                    state_input = false;
                    continue;
                }
                '(' => {
                    button_input = true;
                    continue;
                }
                ')' => {
                    button_input = false;
                    buttons[button_count] = button.clone();
                    button = 0;
                    button_count += 1;
                    continue;
                }
                '{' => {
                    joltage_input = true;
                    idx = 0;
                    continue;
                }
                '}' => {
                    joltage_input = false;
                    continue;
                }
                ',' | ' ' => {
                    continue;
                }
                _ => {}
            }
            assert!(!(state_input & button_input & joltage_input));

            if state_input {
                match c {
                    '.' => {}
                    '#' => {
                        state |= 1 << idx;
                    }
                    _ => panic!("Got a different character while state was supposed to be read"),
                }
                idx += 1;
            } else if button_input {
                button |= 1 << c.to_digit(10).unwrap();
            } else if joltage_input {
                joltage[joltage_count] = c.to_digit(10).unwrap();
                joltage_count += 1;
            }
        }
        machines.push(Machine {
            state: 0,
            buttons: buttons,
            target_state: state,
            _joltage: joltage,
        });
    }

    machines
}

pub fn solve_part1(input: &str) {
    let mut machines = parse(input);
    let min_steps_sum: usize = machines
        .iter_mut()
        .filter_map(|machine| machine.min_steps_to_solve())
        .sum();
    print!(
        "The minimum number of steps needed to put all the machines in the right state is {min_steps_sum}"
    );
}

pub fn solve_part2(_input: &str) {
    print!("Hello from day 10");
}
