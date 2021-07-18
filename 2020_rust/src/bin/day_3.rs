const INPUT: &str = include_str!("../../input/day_3.txt");

fn parse() -> Vec<Vec<bool>> {
    INPUT
        .lines()
        .map(|line| line.chars().map(|c| c == '#').collect())
        .collect()
}

fn solve(trees: &[Vec<bool>], slope: (usize, usize)) -> usize {
    (0..)
        .take_while(|n| n * slope.1 < trees.len())
        .filter(|n| trees[n * slope.1][(n * slope.0) % trees[0].len()])
        .count()
}

fn part1() -> usize {
    solve(&parse(), (3, 1))
}

fn part2() -> usize {
    let trees = parse();
    [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|&slope| solve(&trees, slope))
        .product()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
