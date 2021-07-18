use permutohedron::LexicalPermutation;

const INPUT: &str = include_str!("../../input/day_13.txt");

#[derive(Default, Debug)]
struct Neighbors {
    names: Vec<String>,
    preferences: Vec<i32>,
}

impl Neighbors {
    fn new(include_self: bool) -> Self {
        let mut neighbors = Self::default();
        let mut pairs = vec![];
        for line in INPUT.lines() {
            let words: Vec<_> = line.split_ascii_whitespace().collect();
            let a = neighbors.get_or_add_location(words[0].to_string());
            let b = neighbors
                .get_or_add_location(words.last().unwrap().trim_end_matches(".").to_string());
            let happiness = words[3].parse::<i32>().unwrap();
            let modifier = if words[2] == "gain" { 1 } else { -1 };
            pairs.push((a, b, modifier * happiness));
        }

        if include_self {
            neighbors.names.push("Me".to_string());
        }

        let len = neighbors.len();
        neighbors.preferences.resize(len * len, 0);
        for (a, b, delta) in pairs {
            neighbors.preferences[len * b + a] = delta;
        }
        neighbors
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

    pub fn preference(&self, from: usize, to: usize) -> i32 {
        self.preferences[self.len() * from + to]
    }

    pub fn seating_preference(&self, arrangement: &[usize]) -> i32 {
        (0..arrangement.len())
            .map(|i| {
                self.preference(arrangement[i], arrangement[(i + 1) % arrangement.len()])
                    + self.preference(
                        arrangement[i],
                        arrangement[(i + arrangement.len() - 1) % arrangement.len()],
                    )
            })
            .sum()
    }
}

fn find_best_arrangement(neighbors: &Neighbors) -> i32 {
    let mut arrangement: Vec<usize> = (0..neighbors.len()).collect();
    let mut best = neighbors.seating_preference(&arrangement);
    while arrangement.next_permutation() {
        best = best.max(neighbors.seating_preference(&arrangement));
    }
    best
}

fn part1() -> i32 {
    find_best_arrangement(&Neighbors::new(false))
}

fn part2() -> i32 {
    find_best_arrangement(&Neighbors::new(true))
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
