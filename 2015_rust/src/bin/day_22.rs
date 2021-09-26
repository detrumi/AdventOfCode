use std::collections::BinaryHeap;

use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_22.txt");

fn parse() -> (i32, i32) {
    INPUT
        .trim()
        .lines()
        .map(|line| line.split(": ").nth(1).unwrap().parse::<i32>().unwrap())
        .collect_tuple()
        .unwrap()
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Effect {
    Shield,
    Poison,
    Recharge,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Spell {
    name: &'static str,
    cost: i32,
    damage: i32,
    heal: i32,
    effect: Option<(Effect, i32)>,
}

const SPELLS: [Spell; 5] = [
    Spell {
        name: "Magic Missile",
        cost: 53,
        damage: 4,
        heal: 0,
        effect: None,
    },
    Spell {
        name: "Drain",
        cost: 73,
        damage: 2,
        heal: 2,
        effect: None,
    },
    Spell {
        name: "Shield",
        cost: 113,
        damage: 0,
        heal: 0,
        effect: Some((Effect::Shield, 6)),
    },
    Spell {
        name: "Poison",
        cost: 173,
        damage: 0,
        heal: 0,
        effect: Some((Effect::Poison, 6)),
    },
    Spell {
        name: "Recharge",
        cost: 229,
        damage: 0,
        heal: 0,
        effect: Some((Effect::Recharge, 5)),
    },
];

#[derive(Clone, PartialEq, Eq, Default)]
struct State {
    mana_spent: i32,
    active_effects: Vec<(Effect, i32)>,
    hp: i32,
    mana: i32,
    boss_hp: i32,
    shield: i32,
}

impl State {
    pub fn start_turn(&mut self, is_hard: bool) -> bool {
        self.shield = 0;
        if is_hard {
            self.hp -= 1;
        }
        self.active_effects = self
            .active_effects
            .clone()
            .into_iter()
            .filter_map(|(effect, duration)| {
                match effect {
                    Effect::Shield => self.shield = 7,
                    Effect::Poison => self.boss_hp -= 3,
                    Effect::Recharge => self.mana += 101,
                }
                if duration > 1 {
                    Some((effect, duration - 1))
                } else {
                    None
                }
            })
            .collect();
        self.hp > 0
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.mana_spent.cmp(&self.mana_spent)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn fight(is_hard: bool) -> i32 {
    let (hp, mana) = (50, 500);
    let (boss_hp, boss_damage) = parse();
    let mut states = BinaryHeap::new();
    states.push(State {
        hp,
        mana,
        boss_hp,
        ..State::default()
    });
    let mut best_win = i32::MAX;
    while let Some(mut state) = states.pop() {
        if !state.start_turn(is_hard) {
            continue;
        }
        if state.boss_hp <= 0 {
            if state.mana_spent < best_win {
                best_win = state.mana_spent;
            }
            continue;
        }

        for spell in &SPELLS {
            if spell.cost > state.mana {
                continue;
            }

            let mut new_effects = state.active_effects.clone();
            if let Some((effect, duration)) = &spell.effect {
                if state.active_effects.iter().any(|(e, _)| e == effect) {
                    continue;
                }
                new_effects.push((*effect, *duration));
            }

            let mut new_state = State {
                mana_spent: state.mana_spent + spell.cost,
                active_effects: new_effects,
                hp: state.hp + spell.heal,
                mana: state.mana - spell.cost,
                boss_hp: state.boss_hp - spell.damage,
                shield: 0,
            };

            if !new_state.start_turn(is_hard) {
                continue;
            }
            if new_state.boss_hp <= 0 {
                if new_state.mana_spent < best_win {
                    best_win = new_state.mana_spent;
                }
                continue;
            }

            new_state.hp -= (boss_damage - new_state.shield).max(1);
            if new_state.hp <= 0 {
                continue;
            }

            if new_state.mana_spent < best_win {
                states.push(new_state);
            }
        }
    }
    best_win
}

fn main() {
    println!("Part 1: {}", fight(false));
    println!("Part 2: {}", fight(true));
}
