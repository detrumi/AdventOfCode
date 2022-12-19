use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_19.txt");

fn parse() -> Vec<Vec<u32>> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|n| n.parse().unwrap())
                .collect()
        })
        .collect()
}

fn solve(input: Vec<Vec<u32>>, max_time: usize) -> Vec<u32> {
    let mut results = vec![];
    for line in input.iter() {
        let robot_costs = [
            [line[0], 0, 0, 0],
            [line[1], 0, 0, 0],
            [line[2], line[3], 0, 0],
            [line[4], 0, line[5], 0],
        ];

        let mut best = 0;
        let mut queue = Vec::new();
        queue.push((max_time + 1, [1, 0, 0, 0], [0, 0, 0, 0], [true; 4]));
        while let Some((mut time, robots, mut resources, mut can_buy)) = queue.pop() {
            time -= 1;
            if time == 0 {
                if resources[3] > best {
                    best = resources[3];
                }
                continue;
            }

            for i in 0..4 {
                if can_buy[i] && (0..4).all(|j| resources[j] >= robot_costs[i][j]) {
                    if (0..4).all(|j| resources[j] >= 2 * robot_costs[i][j]) {
                        continue;
                    }
                    let mut new_resources = resources.clone();
                    for j in 0..4 {
                        new_resources[j] -= robot_costs[i][j];
                        new_resources[j] += robots[j];
                    }
                    let mut new_robots = robots.clone();
                    new_robots[i] += 1;
                    queue.push((time, new_robots, new_resources, [true; 4]));
                    can_buy[i] = false;
                }
            }

            for i in 0..4 {
                resources[i] += robots[i];
            }

            queue.push((time, robots, resources, can_buy));
        }
        results.push(best);
    }
    results
}

fn main() {
    let input = parse();
    let part1 = solve(input.clone(), 24)
        .iter()
        .zip(1..)
        .map(|(n, i)| n * i)
        .sum::<u32>();
    println!("Part 1: {}", part1);

    let part2 = solve(input.into_iter().take(3).collect_vec(), 32)
        .iter()
        .product::<u32>();
    println!("Part 2: {}", part2);
}
