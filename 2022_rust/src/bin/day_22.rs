use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_22.txt");

type Pos = (isize, isize);

fn destination(pos: Pos, delta: Pos, maze: &Vec<Vec<bool>>, offsets: &Vec<isize>) -> Pos {
    // Move x
    if delta.0 != 0 {
        let x = pos.0 + delta.0;
        let left = offsets[pos.1 as usize];
        let right = left + maze[pos.1 as usize].len() as isize - 1;
        if x < left {
            (right, pos.1)
        } else if x > right {
            (left, pos.1)
        } else {
            (x, pos.1)
        }
    } else {
        // Move y
        let (top, bottom) = {
            let mut y = pos.1;
            while y > 0
                && (offsets[(y - 1) as usize]
                    ..offsets[(y - 1) as usize] + maze[(y - 1) as usize].len() as isize)
                    .contains(&pos.0)
            {
                y -= 1;
            }
            let top = y;
            y = pos.1;
            while y + 1 < maze.len() as isize
                && (offsets[(y + 1) as usize]
                    ..offsets[(y + 1) as usize] + maze[(y + 1) as usize].len() as isize)
                    .contains(&pos.0)
            {
                y += 1;
            }
            (top, y)
        };
        let y = pos.1 + delta.1;
        if y < top {
            (pos.0, bottom)
        } else if y > bottom {
            (pos.0, top)
        } else {
            (pos.0, y)
        }
    }
}

fn part1() -> isize {
    let (input1, input2) = INPUT.trim_end().split_once("\n\n").unwrap();
    let mut maze = vec![];
    let mut offsets = vec![];
    for line in input1.lines() {
        offsets.push((line.len() - line.trim_start().len()) as isize);
        maze.push(line.trim_start().chars().map(|c| c == '.').collect_vec());
    }
    let mut dir = 1;
    let mut pos = (offsets[0], 0);
    for (is_digit, mut group) in &input2.chars().group_by(|c| c.is_ascii_digit()) {
        if !is_digit {
            let c = group.next().unwrap();
            match c {
                'L' => dir = (dir + 3) % 4,
                'R' => dir = (dir + 1) % 4,
                c => panic!("{c}"),
            }
        } else {
            let steps = group.collect::<String>().parse::<usize>().unwrap();
            let delta = match dir {
                0 => (0, -1),
                1 => (1, 0),
                2 => (0, 1),
                _ => (-1, 0),
            };
            for _step in 0..steps {
                let dest = destination(pos, delta, &maze, &offsets);
                if maze[dest.1 as usize][(dest.0 - offsets[dest.1 as usize]) as usize] {
                    pos = dest;
                }
            }
        }
    }
    let facing = match dir {
        0 => 3,
        1 => 0,
        2 => 1,
        3 => 2,
        _ => panic!(),
    };
    1000 * (pos.1 + 1) + 4 * (pos.0 + 1) + facing
}

fn destination2(
    pos: Pos,
    delta: Pos,
    dir: usize,
    maze: &Vec<Vec<bool>>,
    offsets: &Vec<isize>,
) -> (Pos, usize) {
    let (section_x, section_y) = (pos.0 / 50, pos.1 / 50);
    // Move x
    if delta.0 != 0 {
        let x = pos.0 + delta.0;
        let left = offsets[pos.1 as usize];
        let right = left + maze[pos.1 as usize].len() as isize - 1;
        let py = pos.1 % 50;
        if x < left {
            match section_y {
                0 => ((0, 149 - py), 1), // 3 left reversed
                1 => ((py, 100), 2),     // 3 top normal
                2 => ((50, 49 - py), 1), // 0 left reversed
                _ => ((50 + py, 0), 2),  // 0 top normal
            }
            // (right, pos.1)
        } else if x > right {
            match section_y {
                0 => ((99, 149 - py), 3), // 4 right reversed
                1 => ((100 + py, 49), 0), // 1 bottom normal
                2 => ((149, 49 - py), 3), // 1 right reversed
                _ => ((50 + py, 149), 0), // 4 bottom normal
            }
        } else {
            ((x, pos.1), dir)
        }
    } else {
        // Move y
        let (top, bottom) = {
            let mut y = pos.1;
            while y > 0
                && (offsets[(y - 1) as usize]
                    ..offsets[(y - 1) as usize] + maze[(y - 1) as usize].len() as isize)
                    .contains(&pos.0)
            {
                y -= 1;
            }
            let top = y;
            y = pos.1;
            while y + 1 < maze.len() as isize
                && (offsets[(y + 1) as usize]
                    ..offsets[(y + 1) as usize] + maze[(y + 1) as usize].len() as isize)
                    .contains(&pos.0)
            {
                y += 1;
            }
            (top, y)
        };
        let y = pos.1 + delta.1;
        let px = pos.0 % 50;
        if y < top {
            match section_x {
                0 => ((50, 50 + px), 1), // 2 left normal
                1 => ((0, 150 + px), 1), // 5 left normal
                _ => ((px, 199), 0),     // 5 bottom normal
            }
        } else if y > bottom {
            match section_x {
                0 => ((100 + px, 0), 2),  // 1 top normal
                1 => ((49, 150 + px), 3), // 5 right normal
                _ => ((99, 50 + px), 3),  // 2 right normal
            }
        } else {
            ((pos.0, y), dir)
        }
    }
}

fn part2() -> isize {
    let (input1, input2) = INPUT.trim_end().split_once("\n\n").unwrap();
    let mut maze = vec![];
    let mut offsets = vec![];
    for line in input1.lines() {
        offsets.push((line.len() - line.trim_start().len()) as isize);
        maze.push(line.trim_start().chars().map(|c| c == '.').collect_vec());
    }
    let mut dir = 1;
    let mut pos = (offsets[0], 0);
    for (is_digit, mut group) in &input2.chars().group_by(|c| c.is_ascii_digit()) {
        if !is_digit {
            let c = group.next().unwrap();
            match c {
                'L' => dir = (dir + 3) % 4,
                'R' => dir = (dir + 1) % 4,
                c => panic!("{c}"),
            }
        } else {
            let steps = group.collect::<String>().parse::<usize>().unwrap();
            for _step in 0..steps {
                let delta = match dir {
                    0 => (0, -1),
                    1 => (1, 0),
                    2 => (0, 1),
                    _ => (-1, 0),
                };
                let (dest, dir2) = destination2(pos, delta, dir, &maze, &offsets);
                if maze[dest.1 as usize][(dest.0 - offsets[dest.1 as usize]) as usize] {
                    pos = dest;
                    dir = dir2;
                }
            }
        }
    }
    let facing = match dir {
        0 => 3,
        1 => 0,
        2 => 1,
        3 => 2,
        _ => panic!(),
    };
    1000 * (pos.1 + 1) + 4 * (pos.0 + 1) + facing
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
