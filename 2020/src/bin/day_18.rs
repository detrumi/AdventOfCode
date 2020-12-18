const INPUT: &str = include_str!("../../input/day_18.txt");

#[derive(Debug)]
enum Op {
    Add,
    Mul,
}

use Op::*;

fn solve<S: Into<String>>(input: impl Iterator<Item = S>) -> Option<usize> {
    let mut result = 0;
    for line in input {
        let mut stack = vec![(0, Add)];
        for c in line.into().chars() {
            match c {
                '(' => stack.push((0, Add)),
                ')' => {
                    let (n, _) = stack.pop()?;
                    let (last, op) = stack.last_mut()?;
                    *last = match op {
                        Add => *last + n,
                        Mul => *last * n,
                    }
                }
                '+' => stack.last_mut()?.1 = Add,
                '*' => stack.last_mut()?.1 = Mul,
                ' ' => (),
                _ => {
                    let n = c.to_digit(10)? as usize;
                    let (last, op) = stack.last_mut()?;
                    *last = match op {
                        Add => *last + n,
                        Mul => *last * n,
                    };
                }
            }
        }
        result += stack[0].0;
    }
    Some(result)
}

fn part1() -> Option<usize> {
    solve(INPUT.lines())
}

fn part2() -> Option<usize> {
    solve(INPUT.lines().map(|line| {
        format!(
            "(({}))",
            line.replace('(', "(((")
                .replace(')', ")))")
                .replace('+', ")+(")
                .replace('*', "))*((")
        )
    }))
}

fn main() {
    println!("Part 1: {}", part1().unwrap());
    println!("Part 2: {}", part2().unwrap());
}
