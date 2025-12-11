#[derive(Debug)]
struct Point(usize, usize);
fn parse(input: &str) -> Vec<Point> {
    let mut results = vec![];
    for line in input.lines() {
        if let Some((first, second)) = line.split_once(',') {
            results.push(Point(
                first.parse::<usize>().unwrap(),
                second.parse::<usize>().unwrap(),
            ))
        };
    }
    results
}

fn area(p1: &Point, p2: &Point) -> usize {
    let x_dist = if p1.0 > p2.0 {
        p1.0 - p2.0 + 1
    } else {
        p2.0 - p1.0 + 1
    };
    let y_dist = if p1.1 > p2.1 {
        p1.1 - p2.1 + 1
    } else {
        p2.1 - p1.1 + 1
    };
    return x_dist * y_dist;
}

pub fn solve_part1(input: &str) {
    let corners = parse(input);
    let squares = corners.iter().flat_map(|corner| {
        corners.iter().map(|corner2| {
            ((
                corner.0,
                corner.1,
                corner2.0,
                corner2.1,
                area(corner, corner2),
            ))
        })
    });
    if let Some((c1x, c1y, c2x, c2y, largest)) = squares.max_by_key(|(_, _, _, _, area)| *area) {
        println!(
            "Largest area that can be made is {largest}, using ({c1x}, {c1y}) and ({c2x}, {c2y})"
        );
    }
}

pub fn solve_part2(input: &str) {
    print!("Hello from day 8");
}
