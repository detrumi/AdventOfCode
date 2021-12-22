use itertools::Itertools;

const INPUT: &str = include_str!("../../input/day_22.txt");

type Cuboid = Vec<(isize, isize)>;

fn parse() -> Vec<(Cuboid, bool)> {
    INPUT
        .trim()
        .lines()
        .map(|line| {
            let (command, parts) = line.split_once(' ').unwrap();
            let enable = command == "on";
            let cuboid = parts
                .split(',')
                .map(|s| s.split_once('=').unwrap().1)
                .map(|s| {
                    s.split("..")
                        .map(|s| s.parse().unwrap())
                        .collect_tuple()
                        .unwrap()
                })
                .collect();
            (cuboid, enable)
        })
        .collect()
}

fn overlap(left: &Cuboid, right: &Cuboid) -> Option<Cuboid> {
    let mut result = vec![];
    for i in 0..3 {
        let from = left[i].0.max(right[i].0);
        let to = left[i].1.min(right[i].1);
        if from > to {
            return None;
        }
        result.push((from, to));
    }
    Some(result)
}

fn size(cuboid: &Cuboid) -> isize {
    cuboid.iter().map(|range| range.1 - range.0 + 1).product()
}

fn solve(start_cubes: &[(Cuboid, bool)]) -> isize {
    let mut cubes: Vec<(Cuboid, bool)> = vec![];
    for (new_cube, enable_new) in start_cubes {
        let mut new_cubes = cubes.clone();
        if *enable_new {
            new_cubes.push((new_cube.clone(), true));
        }

        for (old_cube, enable_old) in cubes {
            if let Some(overlap) = overlap(&new_cube, &old_cube) {
                let to_add = if enable_old == *enable_new {
                    !*enable_new
                } else {
                    *enable_new
                };
                new_cubes.push((overlap, to_add));
            }
        }
        cubes = new_cubes;
    }

    cubes
        .into_iter()
        .map(|(cube, b)| size(&cube) * if b { 1 } else { -1 })
        .sum()
}

fn main() {
    let cubes = parse();
    let small_cubes = cubes
        .iter()
        .cloned()
        .take_while(|(c, _)| c[0].0.abs() < 50)
        .collect_vec();

    println!("Part 1: {}", solve(&small_cubes));
    println!("Part 2: {}", solve(&cubes));
}
