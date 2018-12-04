use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use chrono::prelude::*;


#[derive(Hash, Debug)]
struct Event {
    datetime: chrono::DateTime<chrono::offset::Utc>,
    action: Action,
}

#[derive(Hash, Debug)]
enum Action {
    Begin(i32),
    Sleep,
    Wake,
}

fn main() {
    let file = File::open("input/day_4.txt").unwrap();
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect();

    let mut events: Vec<Event> = vec![];
    for line in lines {
        let words: Vec<&str> = line.split(" ").collect();
        let date: Vec<u32> = words[0][1..].split("-").map(|p| p.parse::<u32>().unwrap()).collect();
        let time: Vec<u32> = words[1][0..words[1].len()-1].split(":").map(|p| p.parse::<u32>().unwrap()).collect();

        let action = match words[2] {
            "Guard" => Action::Begin(words[3][1..].parse::<i32>().unwrap()),
            "falls" => Action::Sleep,
            "wakes" => Action::Wake,
            _ => panic!(),
        };

        let datetime = Utc.ymd(date[0] as i32, date[1], date[2]).and_hms(time[0], time[1], 0);
        events.push(Event { datetime, action });
    }

    events.sort_by_key(|e| e.datetime);

    let mut guard = 0;
    let mut total_sleep: HashMap<i32, i32> = HashMap::new();
    let mut most_awake: HashMap<i32, HashMap<i32, i32>> = HashMap::new(); // guard -> minute -> times asleep
    let mut start = Utc::now();
    for e in events {
        match e.action {
            Action::Begin(id) => guard = id,
            Action::Sleep => start = e.datetime,
            Action::Wake => {
                let mut date = start;
                while date < e.datetime {
                    *most_awake.entry(guard).or_default().entry(date.minute() as i32).or_default() += 1;
                    date = date + chrono::Duration::minutes(1);;
                }
                *total_sleep.entry(guard).or_default() += (e.datetime - start).num_minutes() as i32;
            },
        }
    }

    let guard = total_sleep.iter().max_by_key(|kv| kv.1).unwrap().0;
    let min1 = most_awake.get(guard).unwrap().iter().max_by_key(|kv| kv.1).unwrap().0;
    println!("Part 1: {}", guard * *min1 as i32);

    let min2 = most_awake.iter()
        .map(|kv| (kv.0, kv.1.iter().max_by_key(|k| k.1)))
        .max_by_key(|kv| kv.1.unwrap().1).unwrap();
    println!("Part 2: {}", min2.0 * min2.1.unwrap().0);
}
