use std::ops::Add;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_21.txt");

#[derive(Clone, Copy, PartialEq, Eq)]
struct Equipment(i32, i32, i32);

impl Add for Equipment {
    type Output = Equipment;

    fn add(self, rhs: Self) -> Self::Output {
        Equipment(self.0 + rhs.0, self.1 + rhs.1, self.2 + rhs.2)
    }
}

const WEAPONS: [Equipment; 5] = [
    Equipment(8, 4, 0),
    Equipment(10, 5, 0),
    Equipment(25, 6, 0),
    Equipment(40, 7, 0),
    Equipment(74, 8, 0),
];

const ARMOR: [Equipment; 6] = [
    Equipment(0, 0, 0),
    Equipment(13, 0, 1),
    Equipment(31, 0, 2),
    Equipment(53, 0, 3),
    Equipment(75, 0, 4),
    Equipment(102, 0, 5),
];

const RINGS: [Equipment; 7] = [
    Equipment(0, 0, 0),
    Equipment(25, 1, 0),
    Equipment(50, 2, 0),
    Equipment(100, 3, 0),
    Equipment(20, 0, 1),
    Equipment(40, 0, 2),
    Equipment(80, 0, 3),
];

fn parse() -> (i32, i32, i32) {
    INPUT
        .trim()
        .lines()
        .map(|line| line.split(": ").nth(1).unwrap().parse::<i32>().unwrap())
        .collect_tuple()
        .unwrap()
}

fn part1() -> i32 {
    let (boss_hp, boss_dmg, boss_armor) = parse();
    let mut best = i32::MAX;
    for weapon in &WEAPONS {
        for armor in &ARMOR {
            for ring1 in &RINGS {
                for ring2 in &RINGS {
                    if ring1 == ring2 {
                        continue;
                    }
                    let Equipment(cost, dmg, armor) = *weapon + *armor + *ring1 + *ring2;
                    let player_turns = (boss_hp as f32 / (dmg - boss_armor).max(0) as f32).ceil();
                    let boss_turns = (100.0 / (boss_dmg - armor).max(0) as f32).ceil();
                    if player_turns <= boss_turns && cost < best {
                        best = cost;
                    }
                }
            }
        }
    }
    best
}

fn part2() -> i32 {
    let (boss_hp, boss_dmg, boss_armor) = parse();
    let mut worst = i32::MIN;
    for weapon in &WEAPONS {
        for armor in &ARMOR {
            for ring1 in &RINGS {
                for ring2 in &RINGS {
                    if ring1 == ring2 {
                        continue;
                    }

                    let Equipment(cost, dmg, armor) = *weapon + *armor + *ring1 + *ring2;
                    let player_turns = (boss_hp as f32 / (dmg - boss_armor).max(0) as f32).ceil();
                    let boss_turns = (100.0 / (boss_dmg - armor).max(0) as f32).ceil();
                    if player_turns > boss_turns && cost > worst {
                        worst = cost;
                    }
                }
            }
        }
    }
    worst
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
