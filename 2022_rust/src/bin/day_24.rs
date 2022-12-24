use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_24.txt");

struct Maze {
    pub width: usize,
    pub height: usize,
    pub horizontals: Vec<HashMap<(usize, usize), Vec<usize>>>,
    pub verticals: Vec<HashMap<(usize, usize), Vec<usize>>>,
}

impl Maze {
    pub fn new() -> Self {
        let input = INPUT
            .trim()
            .lines()
            .map(|line| line.chars().collect_vec())
            .collect_vec();
        let height = input.len() - 2;
        let width = input[0].len() - 2;
        let mut horizontals: Vec<HashMap<(usize, usize), Vec<usize>>> = vec![HashMap::new()];
        let mut verticals: Vec<HashMap<(usize, usize), Vec<usize>>> = vec![HashMap::new()];
        for y in 0..height {
            for x in 0..width {
                match input[y + 1][x + 1] {
                    '^' => {
                        verticals[0].entry((x, y)).or_default().push(0);
                    }
                    '>' => {
                        horizontals[0].entry((x, y)).or_default().push(1);
                    }
                    'v' => {
                        verticals[0].entry((x, y)).or_default().push(2);
                    }
                    '<' => {
                        horizontals[0].entry((x, y)).or_default().push(3);
                    }
                    _ => (),
                }
            }
        }

        for t in 1..width {
            let mut current: HashMap<(usize, usize), Vec<usize>> = HashMap::new();
            for ((x, y), dirs) in &horizontals[t - 1] {
                for dir in dirs {
                    let x = match dir {
                        1 => (x + 1) % width,
                        _ => (x + width - 1) % width,
                    };
                    current.entry((x, *y)).or_default().push(*dir);
                }
            }
            horizontals.push(current);
        }
        for t in 1..height {
            let mut current: HashMap<(usize, usize), Vec<usize>> = HashMap::new();
            for ((x, y), dirs) in &verticals[t - 1] {
                for dir in dirs {
                    let y = match dir {
                        2 => (y + 1) % height,
                        _ => (y + height - 1) % height,
                    };
                    current.entry((*x, y)).or_default().push(*dir);
                }
            }
            verticals.push(current);
        }

        Self {
            width,
            height,
            horizontals,
            verticals,
        }
    }

    fn safe_at(&self, time: usize, x: usize, y: usize) -> bool {
        !self.horizontals[time % self.width].contains_key(&(x, y))
            && !self.verticals[time % self.height].contains_key(&(x, y))
    }

    fn solve(
        &self,
        start_time: usize,
        (start_x, start_y): (usize, usize),
        end: (usize, usize),
    ) -> usize {
        let mut queue = BinaryHeap::new();
        for time in start_time..start_time + 50 {
            if self.safe_at(time, start_x, start_y) {
                queue.push(State {
                    time,
                    x: start_x,
                    y: start_y,
                });
            }
        }
        let mut visited = HashSet::new();
        while let Some(State { time, x, y }) = queue.pop() {
            if (x, y) == end {
                return time + 1;
            }

            let mut push = |x, y| {
                if self.safe_at(time + 1, x, y) && visited.insert((time + 1, x, y)) {
                    queue.push(State {
                        time: time + 1,
                        x,
                        y,
                    })
                }
            };

            if x > 0 {
                push(x - 1, y)
            }
            if y > 0 {
                push(x, y - 1)
            }
            if x < self.width - 1 {
                push(x + 1, y)
            }
            if y < self.height - 1 {
                push(x, y + 1)
            }

            push(x, y);
        }
        panic!()
    }
}

#[derive(PartialEq, Eq, Debug)]
struct State {
    pub time: usize,
    pub x: usize,
    pub y: usize,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.time.cmp(&self.time)
    }
}

fn main() {
    let maze = Maze::new();
    let start = (0, 0);
    let end = (maze.width - 1, maze.height - 1);
    let trip1 = maze.solve(1, start, end);
    println!("Part 1: {trip1}");

    let trip2 = maze.solve(trip1, end, start);
    let trip3 = maze.solve(trip2, start, end);
    println!("Part 2: {trip3}");
}
