const ROW: usize = 2981;
const COLUMN: usize = 3075;

fn part1() -> usize {
    let (mut x, mut y) = (0, 0);
    let mut n = 20151125;
    while y + 1 != ROW || x + 1 != COLUMN {
        n *= 252533;
        n %= 33554393;
        if y == 0 {
            y = x + 1;
            x = 0;
        } else {
            y -= 1;
            x += 1;
        }
    }
    n
}

fn main() {
    println!("Part 1: {}", part1());
}
