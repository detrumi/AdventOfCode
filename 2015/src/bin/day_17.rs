const INPUT: &str = include_str!("../../input/day_17.txt");

fn combinations(target: usize, containers: &[usize]) -> usize {
    let mut result = 0;
    for i in 0..containers.len() {
        if containers[i] == target {
            result += 1;
        } else if containers[i] < target {
            result += combinations(target - containers[i], &containers[i + 1..])
        }
    }
    result
}

fn combination_sizes(target: usize, depth: usize, containers: &[usize], result: &mut Vec<usize>) {
    for i in 0..containers.len() {
        if containers[i] == target {
            if result.len() < depth {
                result.resize(depth, 0);
            }
            result[depth - 1] += 1;
        } else if containers[i] < target {
            combination_sizes(
                target - containers[i],
                depth + 1,
                &containers[i + 1..],
                result,
            );
        }
    }
}

fn part2(containers: &[usize]) -> usize {
    let mut sizes = vec![];
    combination_sizes(150, 1, containers, &mut sizes);
    *sizes.iter().filter(|&&s| s > 0).next().unwrap()
}

fn main() {
    let mut containers: Vec<usize> = INPUT.lines().map(|l| l.parse().unwrap()).collect();
    containers.sort_by(|a, b| b.cmp(a));
    println!("Part 1: {}", combinations(150, &containers));
    println!("Part 2: {}", part2(&containers));
}
