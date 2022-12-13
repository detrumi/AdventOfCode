use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_13.txt");

fn part1() -> usize {
    let mut result = 0;
    for (section, index) in INPUT.trim().split("\n\n").zip(1..) {
        eprintln!("pair {:?}", index);
        let lines = section
            .lines()
            .map(|line| {
                let mut line = line.to_string();
                line = line.replace(",", " ");
                line = line.replace("[", " [ ");
                line = line.replace("]", " ] ");
                line.push_str(" E");
                line.split_ascii_whitespace()
                    .map(|s| s.to_string())
                    .collect_vec()
            })
            .collect_vec();
        let (mut left, mut right) = (lines[0].iter().peekable(), lines[1].iter().peekable());
        let mut left_lists = 0;
        let mut right_lists = 0;
        'section: loop {
            eprintln!("{:?},{:?}", left.peek(), right.peek());
            match (
                left.peek().unwrap().as_ref(),
                right.peek().unwrap().as_ref(),
            ) {
                ("[", "[") => {
                    left.next();
                    right.next();
                }
                ("]", "]") => {
                    left.next();
                    right.next();
                }
                ("E", _) => {
                    eprintln!("index = {:?}", index);
                    result += index;
                    break;
                }
                (_, "E") => {
                    println!("right E");
                    break;
                }
                ("]", _) => {
                    // Left section ended, skip rest of right list till matching `]`
                    left_lists -= 1;
                    left.next();
                    let mut nesting = 0;
                    loop {
                        if let Some(r) = right.next() {
                            match r.as_ref() {
                                "]" if nesting == 0 => break,
                                "]" => nesting -= 1,
                                "[" => nesting += 1,
                                _ => (),
                            }
                        } else {
                            break 'section;
                        }
                    }
                }
                ("[", "]") => {
                    break;
                    // left.next();
                }
                (_, "]") => {
                    break;
                }
                ("[", _) => {
                    left.next();
                    left_lists += 1;
                }
                (_, "[") => {
                    right.next();
                    right_lists += 1;
                }
                (_, _) => {
                    let l: usize = left.next().unwrap().parse().unwrap();
                    let r: usize = right.next().unwrap().parse().unwrap();
                    if l > r {
                        break;
                    }
                    while left_lists > 0 {
                        let mut nesting = 0;
                        loop {
                            if let Some(l) = left.next() {
                                match l.as_ref() {
                                    "]" if nesting == 0 => break,
                                    "]" => nesting -= 1,
                                    "[" => nesting += 1,
                                    _ => (),
                                }
                            } else {
                                // TODO valid?
                                eprintln!("index = {:?}", index);
                                result += index;
                                break 'section;
                            }
                        }
                        left_lists -= 1;
                    }
                    while right_lists > 0 {
                        let mut nesting = 0;
                        loop {
                            if let Some(r) = right.next() {
                                match r.as_ref() {
                                    "]" if nesting == 0 => break,
                                    "]" => nesting -= 1,
                                    "[" => nesting += 1,
                                    _ => (),
                                }
                            } else {
                                break 'section;
                            }
                        }
                        right_lists -= 1;
                    }
                }
            }
        }
    }
    result
}

fn part1b() -> usize {
    let mut result = 0;
    for (section, index) in INPUT.trim().split("\n\n").zip(1..) {
        eprintln!("pair {:?}", index);
        let lines = section
            .lines()
            .map(|line| {
                let mut line = line.to_string();
                line = line.replace(",", " ");
                line = line.replace("[", " [ ");
                line = line.replace("]", " ] ");
                line.split_ascii_whitespace()
                    .map(|s| s.to_string())
                    .collect_vec()
            })
            .collect_vec();
        let (mut left, mut right) = (lines[0].iter().peekable(), lines[1].iter().peekable());
        'section: loop {
            if left.peek().is_none() {
                eprintln!("index = {:?}", index);
                result += index;
                break;
            }
            if right.peek().is_none() {
                break;
            }
            eprintln!("{:?},{:?}", left.peek(), right.peek());
            match (
                left.peek().unwrap().as_ref(),
                right.peek().unwrap().as_ref(),
            ) {
                ("[", "[") => {
                    left.next();
                    right.next();
                }
                ("]", "]") => {
                    left.next();
                    right.next();
                }
                ("]", _) => {
                    // Left section ended, skip rest of right list till matching `]`
                    left.next();
                    let mut nesting = 0;
                    loop {
                        if let Some(r) = right.next() {
                            match r.as_ref() {
                                "]" if nesting == 0 => break,
                                "]" => nesting -= 1,
                                "[" => nesting += 1,
                                _ => (),
                            }
                        } else {
                            break 'section;
                        }
                    }
                }
                (_, "]") => {
                    // Right section too short
                    break;
                }

                (_, "[") => {
                    // Unwrap right list
                    let l: usize = left.next().unwrap().parse().unwrap();
                    let mut nesting = 0;
                    right.next(); // `[`
                    loop {
                        let Some(r) = right.next() else { panic!("Right list not closed yet") };
                        match r.as_ref() {
                            "[" => nesting += 1,
                            "]" if nesting > 1 => nesting -= 1,
                            "]" => break 'section, // Right list too short
                            _ => {
                                let r: usize = r.parse().unwrap();
                                if l > r {
                                    break 'section;
                                } else if l < r {
                                    result += index;
                                    break 'section;
                                }
                                break;
                            }
                        }
                    }

                    // Go to next `]`
                    loop {
                        let Some(r) = right.next() else { panic!("Right list not closed yet") };
                        match r.as_ref() {
                            "]" if nesting == 0 => break,
                            "]" => nesting -= 1,
                            "[" => nesting += 1,
                            _ => (),
                        }
                    }
                }
                ("[", _) => {
                    // Unwrap left list
                    let r: usize = right.next().unwrap().parse().unwrap();
                    let mut nesting = 0;
                    left.next(); // `[`
                    loop {
                        let Some(l) = left.next() else { panic!("Left list not closed yet") };
                        match l.as_ref() {
                            "[" => nesting += 1,
                            "]" if nesting > 1 => nesting -= 1,
                            "]" => {
                                // Left list ended, continue after list
                                break;
                            }
                            _ => {
                                let l: usize = l.parse().unwrap();
                                if l > r {
                                    break 'section;
                                } else if l < r {
                                    result += index;
                                    break 'section;
                                }
                                break;
                            }
                        }
                    }

                    // Go to next `]`
                    loop {
                        let Some(l) = left.next() else { break };
                        match l.as_ref() {
                            "]" if nesting == 0 => break,
                            "]" => nesting -= 1,
                            "[" => nesting += 1,
                            _ => (),
                        }
                    }
                }
                (_, _) => {
                    let l: usize = left.next().unwrap().parse().unwrap();
                    let r: usize = right.next().unwrap().parse().unwrap();
                    if l > r {
                        break;
                    } else if l < r {
                        result += index;
                        break;
                    }
                }
            }
        }
    }
    result
}

fn part2() -> usize {
    0
}

// Left should run out:
// [[           ],[[],3,[]],[]]
// [[4,1,[3,5,1]]]

fn main() {
    // 4926 wrong
    // 3635 wrong
    // 1669 wrong
    // 1646 wrong
    // 1318 wrong
    // 1248 wrong
    // 1080 wrong
    // 569  ??
    // 456  wrong
    // 329  wrong
    println!("Part 1: {}", part1b());
    println!("Part 2: {}", part2());
}
