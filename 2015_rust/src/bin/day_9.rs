use permutohedron::LexicalPermutation;

const INPUT: &str = include_str!("../../input/day_9.txt");

#[derive(Default, Debug)]
struct Locations {
    names: Vec<String>,
    distances: Vec<u32>,
}

impl Locations {
    fn new() -> Self {
        let mut distances = Self::default();
        let mut pairs = vec![];
        for line in INPUT.lines() {
            let words: Vec<_> = line.split_ascii_whitespace().collect();
            let a = distances.get_or_add_location(words[0].to_string());
            let b = distances.get_or_add_location(words[2].to_string());
            let dist = words[4].parse::<u32>().unwrap();
            pairs.push((a, b, dist));
        }

        let len = distances.names.len();
        distances.distances.resize(len * len, 0);
        for (a, b, dist) in pairs {
            distances.distances[len * a + b] = dist;
            distances.distances[len * b + a] = dist;
        }
        distances
    }

    fn get_or_add_location(&mut self, location: String) -> usize {
        if let Some((i, _)) = self.names.iter().enumerate().find(|(_, l)| **l == location) {
            i
        } else {
            self.names.push(location);
            self.len() - 1
        }
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }

    pub fn distance(&self, from: usize, to: usize) -> u32 {
        self.distances[self.names.len() * from + to]
    }

    pub fn route_distance(&self, route: &[usize]) -> u32 {
        (0..route.len() - 1)
            .map(|i| self.distance(route[i], route[i + 1]))
            .sum()
    }
}

fn part1() -> u32 {
    let locations = Locations::new();
    let mut route: Vec<usize> = (0..locations.len()).collect();
    let mut best_distance = locations.route_distance(&route);
    while route.next_permutation() {
        best_distance = best_distance.min(locations.route_distance(&route));
    }
    best_distance
}

fn part2() -> u32 {
    let locations = Locations::new();
    let mut route: Vec<usize> = (0..locations.len()).collect();
    let mut best_distance = locations.route_distance(&route);
    while route.next_permutation() {
        best_distance = best_distance.max(locations.route_distance(&route));
    }
    best_distance
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
