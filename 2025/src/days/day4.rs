const ACCESSIBILITY_THRESHOLD: usize = 4;

#[derive(PartialEq, Eq, Clone)]
enum Tile {
    Empty,
    Paper,
}
type PaperGrid = Vec<Vec<Tile>>;

trait GridAnalysis {
    fn is_paper_accessible(&self, x: usize, y: usize) -> bool;
    fn count_all_accessible_one_round(&self) -> usize;
    fn count_all_accessible(&self) -> usize;
}

impl GridAnalysis for PaperGrid {
    fn is_paper_accessible(&self, row: usize, col: usize) -> bool {
        /*
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1),           (0, 1),
        (1, -1),  (1, 0),  (1, 1), */
        let dirs = [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ];
        let neighbors_count = dirs
            .iter()
            .filter_map(|(dr, dc)| {
                let new_row = row.checked_add_signed(*dr)?;
                let new_col = col.checked_add_signed(*dc)?;
                self.get(new_row)?.get(new_col)
            })
            .filter(|&tile| *tile == Tile::Paper)
            .count();
        if neighbors_count < ACCESSIBILITY_THRESHOLD {
            return true;
        }
        return false;
    }
    fn count_all_accessible_one_round(&self) -> usize {
        self.iter()
            .enumerate()
            .flat_map(|(row, tiles)| {
                tiles.iter().enumerate().filter(move |(col, tile)| {
                    **tile == Tile::Paper && self.is_paper_accessible(row, *col)
                })
            })
            .count()
    }

    fn count_all_accessible(&self) -> usize {
        let mut total = 0;
        let mut changed;
        let mut grid = self.clone();

        // test
        loop {
            changed = false;
            let snapshot = grid.clone();

            for (row, tiles) in snapshot.iter().enumerate() {
                for (col, tile) in tiles.iter().enumerate() {
                    if *tile == Tile::Paper {
                        if snapshot.is_paper_accessible(row, col) {
                            grid[row][col] = Tile::Empty;
                            total += 1;
                            changed = true;
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }
        total
    }
}

fn parse(input: &str) -> PaperGrid {
    let mut results = vec![];

    for line in input.lines() {
        let row = line
            .chars()
            .map(|tile| -> Tile {
                match tile {
                    '.' => Tile::Empty,
                    '@' => Tile::Paper,
                    _ => panic!("Invalid input"),
                }
            })
            .collect();
        results.push(row)
    }
    results
}

pub fn solve_part1(input: &str) {
    let grid = parse(input);
    let total = grid.count_all_accessible_one_round();
    print!("The number of rolls: {total}");
}

pub fn solve_part2(input: &str) {
    let grid = parse(input);
    let total = grid.count_all_accessible();
    print!("The number of rolls: {total}");
}
