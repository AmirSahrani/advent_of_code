use std::fmt;

#[derive(Clone, Copy)]
enum Tile {
    Empty,
    Split,
    Beam,
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Tile::Empty => write!(f, " "),
            Tile::Split => write!(f, "^"),
            Tile::Beam => write!(f, "|"),
        }
    }
}

fn parse(input: &str) -> Vec<Vec<Tile>> {
    let mut manifold = vec![];
    for line in input.lines() {
        let mut row = vec![];
        for tile in line.chars() {
            row.push(match tile {
                '.' => Tile::Empty,
                'S' => Tile::Beam,
                '^' => Tile::Split,
                _ => panic!("Map only valid for \".S^\""),
            });
        }
        manifold.push(row);
    }
    manifold
}

fn simulate_beam(manifold: &mut Vec<Vec<Tile>>) -> usize {
    let mut beam: Vec<bool> = vec![];
    let mut beam_split_count: usize = 0;
    for tile in manifold.first().unwrap() {
        match tile {
            Tile::Beam => beam.push(true),
            Tile::Split | Tile::Empty => beam.push(false),
        }
    }

    for row in manifold.iter().skip(1) {
        for (i, tile) in row.iter().enumerate() {
            match tile {
                Tile::Split => {
                    if beam[i] {
                        beam_split_count += 1;
                        beam[i + 1] = true;
                        beam[i] = false;
                        beam[i - 1] = true;
                    }
                }
                Tile::Empty => (),
                Tile::Beam => panic!("There should be no beams ahead of the main beam"),
            }
        }
    }

    beam_split_count
}

fn simulate_beam_many_worlds(manifold: &mut Vec<Vec<Tile>>) -> usize {
    let mut manifold_copy = manifold.clone();
    let mut _world_count: usize = 0;
    let mut beam: Vec<bool> = vec![];
    for tile in manifold.first().unwrap() {
        match tile {
            Tile::Beam => beam.push(true),
            Tile::Split | Tile::Empty => beam.push(false),
        }
    }

    for (i, row) in manifold.iter().skip(1).enumerate() {
        for (j, tile) in row.iter().enumerate() {
            match tile {
                Tile::Split => {
                    if beam[j] {
                        beam[j + 1] = true;
                        beam[j] = false;
                        beam[j - 1] = true;
                        // This is not strictly necessary but fun to visualize
                        manifold_copy[i][j + 1] = Tile::Beam;
                        manifold_copy[i][j - 1] = Tile::Beam;
                    }
                }
                Tile::Empty => {
                    if beam[j] {
                        manifold_copy[i][j] = Tile::Beam;
                    }
                }
                Tile::Beam => panic!("There should be no beams ahead of the main beam"),
            }
        }
    }
    0
}

pub fn solve_part1(input: &str) {
    let mut manifold = parse(input);
    let num_beams = simulate_beam(&mut manifold);
    print!("Ended up with {num_beams} splits");
}

pub fn solve_part2(input: &str) {
    let mut manifold = parse(input);
    let num_beams = simulate_beam_many_worlds(&mut manifold);
    print!("Ended up with {num_beams} timelines");
}
