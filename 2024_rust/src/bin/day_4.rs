const INPUT: &str = include_str!("../../input/day_4.txt");

fn parse() -> Vec<Vec<char>> {
    INPUT.trim().lines().map(|l| l.chars().collect()).collect()
}

fn part1() -> usize {
    let input = parse();
    let mut result = 0;
    let end = input.len() as i32 - 1;
    for n in 0..input.len() as i32 {
        result += scan(&input, (0, n), (1, 0)); // right
        result += scan(&input, (end, n), (-1, 0)); // left
        result += scan(&input, (n, 0), (0, 1)); // down
        result += scan(&input, (n, end), (0, -1)); // up

        // upright
        result += scan(&input, (0, n), (1, -1));
        if n != end {
            result += scan(&input, (end - n, end), (1, -1));
        }
        // downright
        result += scan(&input, (n, 0), (1, 1));
        if n != 0 {
            result += scan(&input, (0, n), (1, 1));
        }
        // downleft
        result += scan(&input, (n, 0), (-1, 1));
        if n != end {
            result += scan(&input, (end, end - n), (-1, 1));
        }
        // upleft
        result += scan(&input, (end, n), (-1, -1));
        if n != end {
            result += scan(&input, (n, end), (-1, -1));
        }
    }
    result
}

fn scan(grid: &Vec<Vec<char>>, (mut x, mut y): (i32, i32), (dx, dy): (i32, i32)) -> usize {
    let mut s = vec![];
    loop {
        s.push(grid[y as usize][x as usize]);
        if !(0..grid.len() as i32).contains(&(x + dx))
            || !(0..grid.len() as i32).contains(&(y + dy))
        {
            return s.windows(4).filter(|w| *w == ['X', 'M', 'A', 'S']).count();
        }
        x += dx;
        y += dy;
    }
}

fn part2() -> usize {
    let input = parse();
    let mut result = 0;
    for n in 0..input.len() as i32 {
        result += xscan(&input, (n, 0)); // downright
        if n != 0 {
            result += xscan(&input, (0, n));
        }
    }
    result
}

fn xscan(grid: &Vec<Vec<char>>, (start_x, start_y): (i32, i32)) -> usize {
    let mut x = start_x;
    let mut y = start_y;
    let mut s = vec![];
    let on_grid = 0..grid.len() as i32;
    loop {
        s.push(grid[y as usize][x as usize]);
        if !on_grid.contains(&(x + 1)) || !on_grid.contains(&(y + 1)) {
            let mut result = 0;
            for (n, _) in s
                .windows(3)
                .enumerate()
                .filter(|(_, w)| *w == ['M', 'A', 'S'] || *w == ['S', 'A', 'M'])
            {
                let x = start_x + n as i32;
                let y = start_y + n as i32;
                if !on_grid.contains(&(x + 2)) || !on_grid.contains(&(y + 2)) {
                    break;
                }
                let val_x = grid[y as usize][(x + 2) as usize];
                let val_y = grid[(y + 2) as usize][x as usize];
                if "MS".contains(val_x) && "MS".contains(val_y) && val_x != val_y {
                    result += 1;
                }
            }
            return result;
        }
        x += 1;
        y += 1;
    }
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
