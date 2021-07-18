const INPUT: &str = include_str!("../../input/day_18.txt");

fn parse() -> Vec<Vec<bool>> {
    INPUT
        .lines()
        .map(|line| line.chars().map(|c| c == '#').collect())
        .collect()
}

fn is_corner(x: i32, y: i32, grid: &Vec<Vec<bool>>) -> bool {
    (x == 0 || x == grid[0].len() as i32 - 1) && (y == 0 || y == grid.len() as i32 - 1)
}

fn solve(mut grid: Vec<Vec<bool>>, stuck_corners: bool) -> usize {
    for _step in 0..100 {
        let mut new = grid.clone();
        for y in 0..grid.len() as i32 {
            for x in 0..grid[0].len() as i32 {
                let mut neighbors = 0;
                for dy in -1..=1 {
                    for dx in -1..=1 {
                        if dx != 0 || dy != 0 {
                            neighbors += *grid
                                .get((y + dy) as usize)
                                .and_then(|row| row.get((x + dx) as usize))
                                .unwrap_or(&false)
                                as usize;
                        }
                    }
                }
                new[y as usize][x as usize] = match (stuck_corners, grid[y as usize][x as usize]) {
                    (true, _) if is_corner(x, y, &grid) => true,
                    (_, true) => neighbors == 2 || neighbors == 3,
                    (_, false) => neighbors == 3,
                };
            }
        }
        grid = new;
    }
    grid.iter().map(|r| r.iter().filter(|b| **b).count()).sum()
}

fn main() {
    let grid = parse();
    println!("Part 1: {}", solve(grid.clone(), false));
    println!("Part 2: {}", solve(grid, true));
}
