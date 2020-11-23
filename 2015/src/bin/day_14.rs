const INPUT: &str = include_str!("../../input/day_14.txt");
const RACE_TIME: u32 = 2503;

struct Reindeer {
    speed: u32,
    duration: u32,
    rest_time: u32,
}

impl Reindeer {
    fn parse(line: &str) -> Self {
        let parts: Vec<_> = line.split_ascii_whitespace().collect();
        Self {
            speed: parts[3].parse::<u32>().unwrap(),
            duration: parts[6].parse::<u32>().unwrap(),
            rest_time: parts[13].parse::<u32>().unwrap(),
        }
    }
}

fn part1() -> u32 {
    INPUT
        .lines()
        .map(Reindeer::parse)
        .map(|r| {
            let cycle_time = r.duration + r.rest_time;
            let flying_time =
                r.duration * (RACE_TIME / cycle_time) + (RACE_TIME % cycle_time).min(r.duration);
            r.speed * flying_time
        })
        .max()
        .unwrap()
}

enum State {
    Flying,
    Resting,
}

struct Racer {
    pub state: State,
    pub time_left: u32,
    pub distance: u32,
    pub points: u32,
}

fn part2() -> u32 {
    let reindeer: Vec<_> = INPUT.lines().map(Reindeer::parse).collect();
    let mut racers: Vec<_> = reindeer
        .iter()
        .map(|r| Racer {
            state: State::Flying,
            time_left: r.duration,
            distance: 0,
            points: 0,
        })
        .collect();
    for _ in 0..RACE_TIME {
        for (r, racer) in reindeer.iter().zip(racers.iter_mut()) {
            racer.time_left -= 1;
            match racer.state {
                State::Flying => {
                    racer.distance += r.speed;
                    if racer.time_left == 0 {
                        racer.state = State::Resting;
                        racer.time_left = r.rest_time;
                    }
                }
                State::Resting => {
                    if racer.time_left == 0 {
                        racer.state = State::Flying;
                        racer.time_left = r.duration;
                    }
                }
            }
        }

        let best = racers.iter().map(|racer| racer.distance).max().unwrap();
        racers
            .iter_mut()
            .filter(|racer| racer.distance == best)
            .for_each(|racer| racer.points += 1);
    }
    racers.iter().map(|racer| racer.points).max().unwrap()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
