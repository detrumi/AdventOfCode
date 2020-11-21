const INPUT: &str = include_str!("../../input/day_6.txt");

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
struct Pos {
    x: usize,
    y: usize,
}

impl Pos {
    pub fn parse(s: &str) -> Self {
        let parts: Vec<_> = s.split(",").collect();
        Pos {
            x: parts[0].parse().unwrap(),
            y: parts[1].parse().unwrap(),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
enum Instruction {
    TurnOn,
    TurnOff,
    Toggle,
}

fn parse(line: &str) -> (Instruction, Pos, Pos) {
    let words = line.split(" ").collect::<Vec<_>>();
    let (offset, instruction) = match words[1] {
        "on" => (2, Instruction::TurnOn),
        "off" => (2, Instruction::TurnOff),
        _ => (1, Instruction::Toggle),
    };

    (
        instruction,
        Pos::parse(words[offset]),
        Pos::parse(words[offset + 2]),
    )
}

fn part1() -> usize {
    let mut grid = vec![false; 1000 * 1000];
    for (instruction, from, to) in INPUT.lines().map(parse) {
        for y in from.y..=to.y {
            for x in from.x..=to.x {
                grid[1000 * y + x] = match instruction {
                    Instruction::TurnOn => true,
                    Instruction::TurnOff => false,
                    Instruction::Toggle => !grid[1000 * y + x],
                }
            }
        }
    }

    grid.iter().filter(|&b| *b).count()
}

fn part2() -> i32 {
    let mut grid = vec![0_i32; 1000 * 1000];
    for (instruction, from, to) in INPUT.lines().map(parse) {
        for y in from.y..=to.y {
            for x in from.x..=to.x {
                let old = grid[1000 * y + x];
                grid[1000 * y + x] = match instruction {
                    Instruction::TurnOn => old + 1,
                    Instruction::TurnOff => 0.max(old - 1),
                    Instruction::Toggle => old + 2,
                }
            }
        }
    }

    grid.into_iter().sum()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
