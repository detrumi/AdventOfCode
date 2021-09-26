const INPUT: &str = include_str!("../../input/day_24.txt");

fn parse() -> Vec<usize> {
    INPUT
        .trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

struct Search {
    target_weight: usize,
    best_size: usize,
    best_entanglement: usize,
}

impl Search {
    fn pick_packages(
        &mut self,
        packages: Vec<usize>,
        groups: Vec<Vec<usize>>,
        current_group: usize,
        last_pick: usize,
    ) -> Option<Vec<Vec<usize>>> {
        let mut best = None;
        for i in last_pick..packages.len() {
            let mut packages_left = packages.clone();
            packages_left.remove(i);

            let mut new_groups = groups.clone();
            new_groups[current_group].push(packages[i]);

            let weight = groups[current_group].iter().sum::<usize>() + packages[i];
            let result_groups = match weight.cmp(&self.target_weight) {
                std::cmp::Ordering::Less if new_groups[0].len() < self.best_size => {
                    self.pick_packages(packages_left, new_groups, current_group, i)
                }
                std::cmp::Ordering::Equal => match current_group {
                    g if g == groups.len() - 1 => Some(new_groups),
                    g => self.pick_packages(packages_left, new_groups, g + 1, 0),
                },
                _ => continue,
            };
            if let Some(result_groups) = result_groups {
                let entanglement = result_groups[0].iter().product();
                let is_better = match result_groups[0].len().cmp(&self.best_size) {
                    std::cmp::Ordering::Less => true,
                    std::cmp::Ordering::Equal => entanglement <= self.best_entanglement,
                    std::cmp::Ordering::Greater => false,
                };

                if is_better {
                    self.best_size = result_groups[0].len();
                    self.best_entanglement = entanglement;
                    best = Some(result_groups);
                }
            }
        }
        best
    }
}

fn search(packages: Vec<usize>, num_groups: usize) -> usize {
    let mut search = Search {
        target_weight: packages.iter().sum::<usize>() / num_groups,
        best_size: usize::MAX,
        best_entanglement: usize::MAX,
    };
    let groups = std::iter::repeat(vec![]).take(num_groups - 1).collect();

    let grouping = search.pick_packages(packages, groups, 0, 0);
    grouping.unwrap()[0].iter().product()
}

fn main() {
    let packages = parse();
    println!("Part 1: {}", search(packages.clone(), 3));
    println!("Part 2: {}", search(packages, 4));
}
