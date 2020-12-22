use std::collections::{HashSet, VecDeque};

const INPUT: &str = include_str!("../../input/day_22.txt");

fn parse() -> Vec<VecDeque<usize>> {
    INPUT
        .split("\n\n")
        .map(|chunk| chunk.lines().skip(1).map(|l| l.parse().unwrap()).collect())
        .collect()
}

fn score(deck: &VecDeque<usize>) -> usize {
    deck.iter().rev().zip(1..).map(|(card, i)| i * card).sum()
}

fn part1(mut decks: Vec<VecDeque<usize>>) -> Option<usize> {
    while !decks[0].is_empty() && !decks[1].is_empty() {
        let draw0 = decks[0].pop_front()?;
        let draw1 = decks[1].pop_front()?;
        if draw0 > draw1 {
            decks[0].push_back(draw0);
            decks[0].push_back(draw1);
        } else {
            decks[1].push_back(draw1);
            decks[1].push_back(draw0);
        }
    }
    let winner = decks.iter().find(|d| !d.is_empty())?;
    Some(score(winner))
}

fn part2(mut decks: Vec<VecDeque<usize>>) -> Option<(usize, usize)> {
    let mut previous_decks = vec![HashSet::new(); 2];
    while decks.iter().all(|d| !d.is_empty()) {
        if (0..2).any(|i| !previous_decks[i].insert(decks[i].clone())) {
            return Some((0, score(&decks[0])));
        }

        let draws: Vec<_> = (0..2).map(|i| decks[i].pop_front().unwrap()).collect();
        let winner = if (0..2).all(|i| decks[i].len() >= draws[i]) {
            let mut new_decks = decks.clone();
            (0..2).for_each(|i| new_decks[i].truncate(draws[i]));
            part2(new_decks)?.0
        } else {
            (draws[1] > draws[0]) as usize
        };
        decks[winner].push_back(draws[winner]);
        decks[winner].push_back(draws[1 - winner]);
    }
    let (winner, winning_deck) = decks.iter().enumerate().find(|(_, d)| !d.is_empty())?;
    Some((winner, score(winning_deck)))
}

fn main() {
    let decks = parse();
    println!("Part 1: {}", part1(decks.clone()).unwrap());
    println!("Part 2: {}", part2(decks).unwrap().1);
}
