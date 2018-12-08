use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("input/day_8.txt").unwrap();
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect();

    let mut it = lines[0].split(" ").map(|s| s.parse::<i32>().unwrap());
    println!("Parts 1 and 2: {:?}", walk_tree(&mut it).unwrap());
}

fn walk_tree<'a, I>(it: &mut I) -> Option<(i32, i32)>
    where I: Iterator<Item = i32> {

    let mut result1 = 0;
    let mut result_nodes = vec![];
    let mut result2 = 0;

    let childs = it.next()?;
    let meta = it.next()?;
    for _c in 0..childs {
        let (p1, p2) = walk_tree(it)?;
        result1 += p1;
        result_nodes.push(p2);
    }
    for _m in 0..meta {
        let meta = it.next()?;
        result1 += meta;
        if meta > 0 && meta <= result_nodes.len() as i32 {
            result2 += result_nodes[(meta - 1) as usize];
        }
    }
    Some((result1, if childs == 0 { result1 } else { result2 }))
}
