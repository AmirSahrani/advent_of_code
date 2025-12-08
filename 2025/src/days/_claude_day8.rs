// This file is for me to come back to in order to see how I can improve my quite messy solution to
// day 8
use regex::Regex;
use std::collections::HashSet;

#[derive(Debug)]
struct Point {
    x: f64,
    y: f64,
    z: f64,
}

impl Point {
    fn distance(&self, other: &Point) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2) + (self.z - other.z).powi(2))
            .sqrt()
    }
}

fn parse(input: &str) -> Vec<Point> {
    let re = Regex::new(r"(\d+),(\d+),(\d+)").unwrap();
    re.captures_iter(input)
        .map(|c| {
            let (_, [x, y, z]) = c.extract();
            Point {
                x: x.parse().unwrap(),
                y: y.parse().unwrap(),
                z: z.parse().unwrap(),
            }
        })
        .collect()
}

fn get_all_distances(points: &[Point]) -> Vec<(usize, usize, f64)> {
    let mut distances: Vec<(usize, usize, f64)> = points
        .iter()
        .enumerate()
        .flat_map(|(i, p1)| {
            points[..i]
                .iter()
                .enumerate()
                .map(move |(j, p2)| (i, j, p1.distance(p2)))
        })
        .collect();

    distances.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap());
    distances
}

#[derive(Debug)]
struct Circuit {
    boxes: HashSet<usize>,
}

impl Circuit {
    fn new(i: usize, j: usize) -> Self {
        let mut boxes = HashSet::new();
        boxes.insert(i);
        boxes.insert(j);
        Circuit { boxes }
    }

    fn contains(&self, i: usize) -> bool {
        self.boxes.contains(&i)
    }

    fn insert(&mut self, i: usize) {
        self.boxes.insert(i);
    }

    fn merge(&mut self, other: &Circuit) {
        self.boxes.extend(&other.boxes);
    }

    fn clear(&mut self) {
        self.boxes.clear();
    }

    fn is_empty(&self) -> bool {
        self.boxes.is_empty()
    }

    fn len(&self) -> usize {
        self.boxes.len()
    }
}

struct UnionFind {
    circuits: Vec<Circuit>,
}

impl UnionFind {
    fn new() -> Self {
        UnionFind { circuits: vec![] }
    }

    fn add_edge(&mut self, i: usize, j: usize) {
        let circuit_i = self.find_circuit_containing(i);
        let circuit_j = self.find_circuit_containing(j);

        match (circuit_i, circuit_j) {
            (Some(id_i), Some(id_j)) if id_i == id_j => {
                // Both in same circuit, do nothing
            }
            (Some(id_i), Some(id_j)) => {
                // Merge two different circuits
                self.merge_circuits(id_i, id_j);
            }
            (Some(id), None) => {
                // Only i in a circuit, add j
                self.circuits[id].insert(j);
            }
            (None, Some(id)) => {
                // Only j in a circuit, add i
                self.circuits[id].insert(i);
            }
            (None, None) => {
                // Create new circuit
                self.circuits.push(Circuit::new(i, j));
            }
        }
    }

    fn find_circuit_containing(&self, node: usize) -> Option<usize> {
        self.circuits
            .iter()
            .position(|circuit| circuit.contains(node))
    }

    fn merge_circuits(&mut self, id1: usize, id2: usize) {
        let (id_i, id_j) = if id1 < id2 { (id1, id2) } else { (id2, id1) };
        let (left, right) = self.circuits.split_at_mut(id_j);
        left[id_i].merge(&right[0]);
        right[0].clear();
    }

    fn get_circuits(self) -> Vec<Circuit> {
        self.circuits
            .into_iter()
            .filter(|c| !c.is_empty())
            .collect()
    }

    fn largest_circuit_size(&self) -> usize {
        self.circuits.iter().map(|c| c.len()).max().unwrap_or(0)
    }
}

fn build_circuits_until(
    distances: &[(usize, usize, f64)],
    stop_condition: impl Fn(&UnionFind) -> bool,
) -> (UnionFind, Option<(usize, usize)>) {
    let mut uf = UnionFind::new();

    for &(i, j, _) in distances {
        uf.add_edge(i, j);

        if stop_condition(&uf) {
            return (uf, Some((i, j)));
        }
    }

    (uf, None)
}

pub fn solve_part1(input: &str) {
    let points = parse(input);
    let distances = get_all_distances(&points);

    let num_edges = if points.len() == 20 { 10 } else { 1000 };
    let (uf, _) = build_circuits_until(&distances[..num_edges], |_| false);

    let mut sizes: Vec<usize> = uf.get_circuits().iter().map(|c| c.len()).collect();

    sizes.sort_unstable_by(|a, b| b.cmp(a));

    let product: usize = sizes.iter().take(3).product();
    println!("The product of the three largest circuits is {}", product);
}

pub fn solve_part2(input: &str) {
    let points = parse(input);
    let distances = get_all_distances(&points);

    let (_, last_edge) =
        build_circuits_until(&distances, |uf| uf.largest_circuit_size() == points.len());

    if let Some((i, j)) = last_edge {
        let product = points[i].x * points[j].x;
        println!(
            "The product of {} and {} is {}",
            points[i].x, points[j].x, product
        );
    }
}

