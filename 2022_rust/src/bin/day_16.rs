use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};

use itertools::Itertools;
use rayon::prelude::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};

const INPUT: &str = include_str!("../../input/day_16.txt");

#[derive(Clone, PartialEq, Eq, Default, Debug)]
struct State {
    pub pressure: usize,
    pub time: usize,
    pub current: String,
    pub opened: HashSet<String>,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.pressure.cmp(&other.pressure)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn part1() -> usize {
    let input: HashMap<String, (usize, Vec<String>)> = INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts = line.split_ascii_whitespace().collect_vec();
            let valve = parts[0].to_string();
            let rate = parts[1].parse().unwrap();
            let tunnels = parts[2..].iter().map(|s| s.to_string()).collect_vec();
            (valve, (rate, tunnels))
        })
        .collect();

    let distances: HashMap<String, HashMap<String, usize>> = input
        .keys()
        .map(|start| {
            let mut map = HashMap::new();
            map.insert(start.clone(), 0);
            let mut queue = VecDeque::new();
            queue.push_back((start, 0));
            while let Some((middle, distance)) = queue.pop_front() {
                for end in &input[middle].1 {
                    if !map.contains_key(end) {
                        queue.push_back((end, distance + 1));
                        map.insert(end.clone(), distance + 1);
                    }
                }
            }
            (start.clone(), map)
        })
        .collect();
    // for (s, d) in &distances {
    //     println!("{s}: {d:?}");
    // }

    let mut queue: BinaryHeap<State> = BinaryHeap::new();
    queue.push(State {
        pressure: 0,
        time: 0,
        current: "AA".to_string(),
        opened: HashSet::new(),
    });
    // for (a, b) in &input {
    //     println!("{a} {b:?}");
    // }
    // println!();

    let mut result = 0;
    while let Some(State {
        pressure,
        time,
        current,
        opened,
    }) = queue.pop()
    {
        // println!("{current}");

        for (tunnel, distance) in &distances[&current] {
            if input[tunnel].0 == 0 || opened.contains(tunnel) {
                continue;
            }

            // Move to valve
            let mut new_pressure = pressure;
            // let mut rate = 0;
            for valve in &opened {
                new_pressure += input[valve].0 * (distance + 1);
                // rate += input[valve].0;
            }

            // eprintln!("(rate, time, opened) = {:?}", (rate, time, &opened));
            if time + distance >= 30 {
                continue;
            }

            let mut new_opened = opened.clone();
            new_opened.insert(tunnel.clone());
            // eprintln!(
            //     "{current}=>{tunnel} {time} ({rate}: {:?})",
            //     opened.iter().sorted().collect_vec()
            // );

            queue.push(State {
                pressure: new_pressure,
                time: time + distance + 1,
                current: tunnel.clone(),
                opened: new_opened.clone(),
            });

            // Wait
            for valve in &new_opened {
                new_pressure += input[valve].0 * (30 - (time + distance + 1));
            }
            if new_pressure > result {
                result = new_pressure;
                // eprintln!("n = {:?} ({new_opened:?})", new_opened);
            }
        }
    }
    result
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
struct State2 {
    pub pressure: usize,
    pub time: usize,
    pub current: (String, String),
    pub steps_left: (usize, usize),
    pub opened: HashSet<String>,
    pub releases: Vec<usize>,
}

impl Ord for State2 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.pressure.cmp(&other.pressure)
    }
}

impl PartialOrd for State2 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn part2() -> usize {
    let input: HashMap<String, (usize, Vec<String>)> = INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts = line.split_ascii_whitespace().collect_vec();
            let valve = parts[0].to_string();
            let rate = parts[1].parse().unwrap();
            let tunnels = parts[2..].iter().map(|s| s.to_string()).collect_vec();
            (valve, (rate, tunnels))
        })
        .collect();

    let distances: HashMap<String, HashMap<String, usize>> = input
        .keys()
        .map(|start| {
            let mut map = HashMap::new();
            map.insert(start.clone(), 0);
            let mut queue = VecDeque::new();
            queue.push_back((start, 0));
            while let Some((middle, distance)) = queue.pop_front() {
                for end in &input[middle].1 {
                    if !map.contains_key(end) {
                        queue.push_back((end, distance + 1));
                        map.insert(end.clone(), distance + 1);
                    }
                }
            }
            (start.clone(), map)
        })
        .collect();

    for d in &distances {
        eprintln!("d = {:?}", d);
    }

    let mut queue: BinaryHeap<State2> = BinaryHeap::new();
    queue.push(State2 {
        pressure: 0,
        time: 0,
        current: ("AA".to_string(), "AA".to_string()),
        steps_left: (0, 0),
        opened: HashSet::new(),
        releases: vec![],
    });

    let total_steps = 30;
    let mut result = 0;
    while let Some(State2 {
        mut pressure,
        time,
        current,
        mut steps_left,
        mut opened,
        mut releases,
    }) = queue.pop()
    {
        if time >= total_steps {
            continue;
        }

        for valve in &opened {
            pressure += input[valve].0;
        }

        if steps_left.0 > 0 {
            steps_left.0 -= 1;
        }
        if steps_left.0 == 0 {
            opened.insert(current.0.clone());
        }
        if steps_left.1 > 0 {
            steps_left.1 -= 1;
        }
        if steps_left.1 == 0 {
            opened.insert(current.1.clone());
        }

        {
            let mut new_pressure = pressure;
            let mut flow = 0;
            for valve in &opened {
                new_pressure += input[valve].0 * (total_steps - time);
                flow += input[valve].0;
                // if total_steps >= time + 3 {
                //     new_pressure += input[valve].0 * (total_steps - time - 3);
                // }
            }
            releases.push(flow);
            if new_pressure > result {
                eprintln!("opened = {:?}", opened);
                eprintln!("flow = {:?}", flow);
                eprintln!("releases = {:?}", releases);
                eprintln!("result = {:?}", new_pressure);
                result = new_pressure;
            }
        }

        match (steps_left.0 == 0, steps_left.1 == 0) {
            // (true, true) => {
            //     //
            // }
            (true, _) => {
                for (tunnel, distance) in &distances[&current.0] {
                    if input[tunnel].0 == 0
                        || opened.contains(tunnel)
                        || time + distance >= total_steps
                    {
                        continue;
                    }

                    // eprintln!("distance = {:?}", distance);
                    let delta = if current.0 == "AA" { 2 } else { 3 };
                    queue.push(State2 {
                        pressure,
                        time: time + 1,
                        current: (tunnel.clone(), current.1.clone()),
                        steps_left: (*distance + delta, steps_left.1), // TODO right -1
                        opened: opened.clone(),
                        releases: releases.clone(),
                    });
                }
            }
            (false, _) => {
                // tmp
                queue.push(State2 {
                    pressure,
                    time: time + 1,
                    current,
                    steps_left: (steps_left.0 - 1, steps_left.1), // TODO right -1
                    opened: opened.clone(),
                    releases,
                });
            }
            // (false, true) => {
            //     //
            // }
            // _ => (),
        }
    }
    result
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
struct Player {
    pub current: String,
    pub visits: Vec<String>,
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
struct State3 {
    pub pressure: usize,
    pub time: usize,
    pub players: [Player; 2],
    pub opened: HashSet<String>,
    // pub releases: Vec<usize>,
}

impl Ord for State3 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // self.pressure.cmp(&other.pressure)
        other
            .time
            .cmp(&self.time)
            .then(self.pressure.cmp(&other.pressure))
        // let key = |s: &Self| s.pressure - 10 * s.time;
        // key(self).cmp(&key(other))
    }
}

impl PartialOrd for State3 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn part2b() -> usize {
    let input: HashMap<String, (usize, Vec<String>)> = INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts = line.split_ascii_whitespace().collect_vec();
            let valve = parts[0].to_string();
            let rate = parts[1].parse().unwrap();
            let tunnels = parts[2..].iter().map(|s| s.to_string()).collect_vec();
            (valve, (rate, tunnels))
        })
        .collect();

    // let mut queue: BinaryHeap<State3> = BinaryHeap::new();
    let mut queue: VecDeque<State3> = VecDeque::new();
    queue.push_back(State3 {
        pressure: 0,
        time: 0,
        players: [
            Player {
                current: "AA".to_string(),
                visits: vec![],
            },
            Player {
                current: "AA".to_string(),
                visits: vec![],
            },
        ],
        opened: HashSet::new(),
        // releases: vec![],
    });
    // let mut visited = HashSet::new();

    let total_steps = 26;
    let mut result = 0;
    while let Some(State3 {
        mut pressure,
        time,
        players,
        opened,
        // mut releases,
    }) = queue.pop_front()
    {
        if time >= total_steps {
            continue;
        }

        for valve in &opened {
            pressure += input[valve].0;
        }

        {
            let mut new_pressure = pressure;
            let mut flow = 0;
            for valve in &opened {
                new_pressure += input[valve].0 * (total_steps - time - 1);
                flow += input[valve].0;
            }
            // releases.push(flow);
            if new_pressure > result {
                eprintln!("opened = {:?}", opened);
                eprintln!("flow = {:?}", flow);
                // eprintln!("releases = {:?}", releases);
                eprintln!("result = {:?}", new_pressure);
                result = new_pressure;
            }
        }

        let mut moves: [Vec<Player>; 2] = [vec![], vec![]];

        for player in 0..2 {
            let Player { current, visits } = &players[player];
            let (rate, tunnels) = &input[current];

            // Open
            if *rate > 0 && !opened.contains(current) {
                moves[player].push(Player {
                    current: current.clone(),
                    visits: vec![],
                });
            }

            // Open
            for tunnel in tunnels {
                if !visits.contains(tunnel) {
                    let mut new_visits = visits.clone();
                    new_visits.push(tunnel.clone());

                    moves[player].push(Player {
                        current: tunnel.clone(),
                        visits: new_visits,
                    })
                }
            }
            // Do nothing
            if moves[player].is_empty() {
                moves[player].push(players[player].clone());
            }
        }

        if moves.iter().any(|m| m.len() > 1) {
            for (move0, move1) in moves[0].iter().cartesian_product(moves[1].iter()) {
                let mut new_opened = opened.clone();
                if move0.current == players[0].current {
                    new_opened.insert(players[0].current.clone());
                }
                if move1.current == players[1].current {
                    new_opened.insert(players[1].current.clone());
                }
                queue.push_back(State3 {
                    pressure,
                    time: time + 1,
                    players: [move0.clone(), move1.clone()],
                    opened: new_opened,
                    // releases: releases.clone(),
                });
            }
        }
    }

    result
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
struct Player2 {
    pub current: String,
    pub visits: Vec<String>,
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
struct State4 {
    pub pressure: usize,
    pub time: usize,
    pub players: [Player2; 2],
    pub opened: HashSet<String>,
    // pub releases: Vec<usize>,
}

const TOTAL_STEPS: usize = 26;

fn part2c() -> usize {
    let input: HashMap<String, (usize, Vec<String>)> = INPUT
        .trim()
        .lines()
        .map(|line| {
            let parts = line.split_ascii_whitespace().collect_vec();
            let valve = parts[0].to_string();
            let rate = parts[1].parse().unwrap();
            let tunnels = parts[2..].iter().map(|s| s.to_string()).collect_vec();
            (valve, (rate, tunnels))
        })
        .collect();

    let state = State4 {
        pressure: 0,
        time: 0,
        players: [
            Player2 {
                current: "AA".to_string(),
                visits: vec![],
            },
            Player2 {
                current: "AA".to_string(),
                visits: vec![],
            },
        ],
        opened: HashSet::new(),
        // releases: vec![],
    };

    search(state, 0, 0, &input)
}

fn search(
    state: State4,
    mut best: usize,
    depth: usize,
    input: &HashMap<String, (usize, Vec<String>)>,
) -> usize {
    let State4 {
        mut pressure,
        time,
        players,
        opened,
        // mut releases,
    } = state;
    if time >= TOTAL_STEPS {
        return 0;
    }

    for valve in &opened {
        pressure += input[valve].0;
    }

    {
        let mut new_pressure = pressure;
        let mut flow = 0;
        for valve in &opened {
            new_pressure += input[valve].0 * (TOTAL_STEPS - time - 1);
            flow += input[valve].0;
        }
        // releases.push(flow);
        if new_pressure > best {
            eprintln!("opened = {:?}", opened);
            eprintln!("flow = {:?}", flow);
            // eprintln!("releases = {:?}", releases);
            eprintln!("best = {:?}", new_pressure);
            best = new_pressure;
        }
    }

    let mut moves: [Vec<Player2>; 2] = [vec![], vec![]];

    for player in 0..2 {
        let Player2 { current, visits } = &players[player];
        let (rate, tunnels) = &input[current];

        // Open
        if *rate > 0 && !opened.contains(current) {
            moves[player].push(Player2 {
                current: current.clone(),
                visits: vec![],
            });
        }

        // Open
        for tunnel in tunnels {
            if !visits.contains(tunnel) {
                let mut new_visits = visits.clone();
                new_visits.push(tunnel.clone());

                moves[player].push(Player2 {
                    current: tunnel.clone(),
                    visits: new_visits,
                })
            }
        }

        // Do nothing
        if moves[player].is_empty() {
            moves[player].push(players[player].clone());
        }
    }

    if moves.iter().any(|m| m.len() > 1) {
        if depth == 0 {
            let all_moves = moves[0]
                .iter()
                .cartesian_product(moves[1].iter())
                .collect_vec();
            let result = all_moves
                .into_par_iter()
                .map(|(move0, move1)| {
                    let mut new_opened = opened.clone();
                    if move0.current == players[0].current {
                        new_opened.insert(players[0].current.clone());
                    }
                    if move1.current == players[1].current {
                        new_opened.insert(players[1].current.clone());
                    }
                    let state = State4 {
                        pressure,
                        time: time + 1,
                        players: [move0.clone(), move1.clone()],
                        opened: new_opened,
                        // releases: releases.clone(),
                    };
                    search(state, best, depth + 1, input)
                })
                .max()
                .unwrap();
            best = best.max(result);
        } else {
            for (move0, move1) in moves[0].iter().cartesian_product(moves[1].iter()) {
                let mut new_opened = opened.clone();
                if move0.current == players[0].current {
                    new_opened.insert(players[0].current.clone());
                }
                if move1.current == players[1].current {
                    new_opened.insert(players[1].current.clone());
                }
                let state = State4 {
                    pressure,
                    time: time + 1,
                    players: [move0.clone(), move1.clone()],
                    opened: new_opened,
                    // releases: releases.clone(),
                };
                best = best.max(search(state, best, depth + 1, input));
            }
        }
    }
    best
}

fn main() {
    // println!("Part 1: {}", part1());
    println!("Part 2: {}", part2c());
}

/*
AA 0 DD II BB
BB 13 CC AA
CC 2 DD BB
DD 20 CC AA EE
EE 3 FF DD
FF 0 EE GG
GG 0 FF HH
HH 22 GG
II 0 AA JJ
JJ 21 II


OQ 17 NB AK KL
HP 0 ZX KQ
GO 0 HR GW
PD 9 XN EV QE MW
NQ 0 HX ZX
DW 0 IR WE
TN 24 KL EI
JJ 0 EV HR
KH 0 ZQ AA
PH 0 FN QE
FD 0 SM HX
SM 7 WW RZ FD HO KQ
PU 0 VL IR
OM 0 CM AA
KX 20 PC
IR 3 PU CM WW DW AF
XG 0 RX OF
QE 0 PH PD
GW 0 JQ GO
HO 0 SM TY
WU 0 SG RZ
MS 0 UE OF
JS 0 DO ZX
YQ 0 BC SG
EJ 0 AA LR
EI 0 BV TN
NC 0 TS BC
AF 0 IR HX
OX 0 HR BV
BF 0 JQ SY
CA 0 YD HX
KQ 0 HP SM
NB 0 OQ OF
SY 0 BF BV
AA 0 KH EJ OM TY DO
BC 11 WE RX YQ LR NC
HR 14 OX GO JJ
WE 0 DW BC
MW 0 JQ PD
DO 0 JS AA
PC 0 AK KX
YD 0 CA OF
RX 0 XG BC
CM 0 IR OM
HX 6 ZQ NQ AF FD CA
ZQ 0 KH HX
BV 21 SY OX EI
AK 0 PC OQ
UE 0 MS JQ
LR 0 BC EJ
JQ 8 MW UE BF GW
VL 0 PU ZX
EV 0 JJ PD
TS 0 NC ZX
RZ 0 SM WU
OF 13 XG YD NB MS XN
WW 0 SM IR
TY 0 HO AA
XN 0 OF PD
SG 15 WU YQ
FN 25 PH
KL 0 TN OQ
ZX 5 JS HP VL NQ TS


*/
