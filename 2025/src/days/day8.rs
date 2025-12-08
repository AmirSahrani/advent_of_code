use std::collections::HashSet;

use regex::Regex;

struct Point {
    x: i32,
    y: i32,
    z: i32,
}

impl Point {
    fn distance(&self, other: &Point) -> f64 {
        Into::<f64>::into(
            (self.x - other.x).pow(2) + (self.y - other.y).pow(2) + (self.z - other.z).pow(2),
        )
        .sqrt()
    }
}
fn parse(input: &str) -> Vec<Point> {
    let mut boxes = vec![];
    let re = Regex::new(r"(\d+),(\d+),(\d+)").unwrap();
    for (_, [x, y, z]) in re.captures_iter(input).map(|c| c.extract()) {
        boxes.push(Point {
            x: x.parse::<i32>().unwrap(),
            y: y.parse::<i32>().unwrap(),
            z: z.parse::<i32>().unwrap(),
        });
    }
    boxes
}

fn get_all_distances(points: &Vec<Point>) -> Vec<Vec<f64>> {
    let mut distances: Vec<Vec<f64>> = vec![];
    for i in 0..points.len() {
        let mut row = vec![];
        for j in 0..i {
            row.push(points[i].distance(&points[j]))
        }
        distances.push(row);
    }
    distances
}

fn make_circuits(distances: Vec<Vec<f64>>) -> Vec<HashSet<usize>> {}

pub fn solve_part1(input: &str) {
    let points = parse(input);
    let distances = get_all_distances(&points);
    println!("{:?}", distances)
}

pub fn solve_part2(input: &str) {
    let _points = parse(input);
    print!("Hello from day 8");
}
