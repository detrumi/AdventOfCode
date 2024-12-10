#![feature(let_chains)]

use std::iter;

const INPUT: &str = include_str!("../../input/day_9.txt");

fn parse() -> Vec<Option<usize>> {
    let mut it = INPUT.trim().chars();
    let mut disk = vec![];
    let mut i = 0;
    while let Some(len) = it.next() {
        disk.extend(iter::repeat_n(Some(i), to_num(len)));
        i += 1;
        if let Some(free) = it.next() {
            disk.extend(iter::repeat_n(None, to_num(free)));
        } else {
            break;
        }
    }
    disk
}

fn part1() -> usize {
    let mut disk = parse();
    let mut head = 0;
    while disk.contains(&None) {
        let Some(n) = disk.pop().unwrap() else {
            continue;
        };
        while disk[head].is_some() {
            head += 1;
        }
        disk[head] = Some(n);
    }
    disk.iter().enumerate().map(|(i, n)| i * n.unwrap()).sum()
}

fn to_num(c: char) -> usize {
    c.to_string().parse().unwrap()
}

#[derive(Debug)]
enum Block {
    Filled(usize, usize),
    Free(usize),
}

fn part2() -> usize {
    let mut it = INPUT.trim().chars();
    let mut disk = vec![];
    let mut i = 0;
    while let Some(len) = it.next() {
        disk.push(Block::Filled(to_num(len), i));
        i += 1;
        if let Some(free) = it.next() {
            disk.push(Block::Free(to_num(free)));
        } else {
            break;
        }
    }
    let mut head = disk.len() - 1;
    while head > 0 {
        let Block::Filled(len, id) = disk[head] else {
            head -= 1;
            continue;
        };
        for i in 0..head {
            if let Block::Free(free) = disk[i]
                && free >= len
            {
                disk[head] = Block::Free(len);
                disk.insert(i, Block::Filled(len, id));
                if len < free {
                    disk[i + 1] = Block::Free(free - len);
                } else {
                    disk.remove(i + 1);
                    head -= 1;
                }
                break;
            }
        }
        head -= 1;
    }
    let mut n = 0;
    let mut result = 0;
    for d in &disk {
        match d {
            &Block::Filled(len, id) => {
                for _ in 0..len {
                    result += n * id;
                    n += 1;
                }
            }
            &Block::Free(len) => n += len,
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
