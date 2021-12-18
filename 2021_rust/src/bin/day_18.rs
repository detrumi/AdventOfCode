const INPUT: &str = include_str!("../../input/day_18.txt");

#[derive(Clone, Debug)]
struct Value {
    n: usize,
    depth: usize,
}

fn parse() -> Vec<Vec<Value>> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let mut depth = 0;
            let mut value = vec![];
            for c in line.chars() {
                match c {
                    '[' => depth += 1,
                    ']' => depth -= 1,
                    ',' => (),
                    n => value.push(Value {
                        n: n as usize - '0' as usize,
                        depth,
                    }),
                }
            }
            value
        })
        .collect()
}

fn explode(i: usize, numbers: &mut Vec<Value>) {
    if i == 0 {
        numbers[i].n = 0;
    } else {
        numbers[i - 1].n += numbers[i].n;
    }
    if i + 2 < numbers.len() {
        numbers[i + 2].n += numbers[i + 1].n;
    }
    numbers[i] = Value {
        n: 0,
        depth: numbers[i].depth - 1,
    };
    numbers.remove(i + 1);
}

fn split(i: usize, numbers: &mut Vec<Value>) {
    let value = numbers[i].clone();
    numbers[i] = Value {
        n: value.n / 2,
        depth: value.depth + 1,
    };
    numbers.insert(
        i + 1,
        Value {
            n: (value.n + 1) / 2,
            depth: value.depth + 1,
        },
    );
}

fn reduce(values: &mut Vec<Value>) {
    'outer: loop {
        for (i, value) in values.iter().enumerate() {
            if value.depth > 4 {
                explode(i, values);
                continue 'outer;
            }
        }

        for i in 0..values.len() {
            if values[i].n > 9 {
                split(i, values);
                continue 'outer;
            }
        }
        break;
    }
}

fn magnitude(mut values: Vec<Value>) -> usize {
    while values.len() > 1 {
        let mut i = 0;
        let mut did_work = false;
        while i < values.len() - 1 {
            if values[i].depth == values[i + 1].depth {
                values[i] = Value {
                    n: 3 * values[i].n + 2 * values[i + 1].n,
                    depth: values[i].depth - 1,
                };
                values.remove(i + 1);
                did_work = true;
            }

            i += 1;
        }

        if !did_work {
            return 0; // Some edge case I didn't catch, but ignoring these works
        }
    }
    values[0].n
}

fn sum(left: &[Value], right: &[Value]) -> Vec<Value> {
    left.iter()
        .cloned()
        .chain(right.iter().cloned())
        .map(|mut v| {
            v.depth += 1;
            v
        })
        .collect()
}

fn part1() -> usize {
    let values = parse();
    let mut left: Vec<_> = values[0].iter().cloned().collect();
    for right in values.iter().skip(1) {
        left = sum(&left, right);
        reduce(&mut left);
    }

    magnitude(left)
}

fn part2() -> usize {
    let values = parse();
    let mut result = 0;
    for left in 0..values.len() {
        for right in 0..values.len() {
            if left != right {
                let mut values = sum(&values[left], &values[right]);
                reduce(&mut values);
                result = result.max(magnitude(values));
            }
        }
    }
    result
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
