use itertools::Itertools;

fn main() {
    let mut part_1 = 0;
    let mut part_2 = 0;
    for num in 382_345..843_167 {
        let digits: Vec<u32> = num
            .to_string()
            .chars()
            .map(|c| c.to_digit(10).unwrap())
            .collect();

        if digits.iter().tuple_windows().all(|(a, b)| a <= b) {
            let groups: Vec<_> = digits
                .into_iter()
                .group_by(|n| *n)
                .into_iter()
                .map(|(_n, g)| g.count())
                .collect();

            if groups.iter().any(|count| *count >= 2) {
                part_1 += 1;
            }
            if groups.iter().any(|count| *count == 2) {
                part_2 += 1;
            }
        }
    }
    println!("part_1 = {:?}", part_1);
    println!("part_2 = {:?}", part_2);
}
