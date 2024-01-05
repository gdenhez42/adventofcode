use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    // Parse input
    let mut bricks = Vec::new();
    for line in contents.lines() {
        let parsed = line.split('~').map(|s| s.split(',').map(|i| i.parse::<u64>().unwrap()).collect::<Vec<u64>>()).collect::<Vec<Vec<u64>>>();
        bricks.push(Brick {
            x1 : parsed[0][0],
            y1 : parsed[0][1],
            z1 : parsed[0][2],
            x2 : parsed[1][0],
            y2 : parsed[1][1],
            z2 : parsed[1][2],
        })
    }

    // Make bricks fall
    bricks.sort_by(|b1, b2| b1.z1.cmp(&b2.z1));
    let mut floors = HashMap::new();
    let mut supports : HashMap<usize, HashSet<usize>> = HashMap::new();
    let mut supported_by : HashMap<usize, HashSet<usize>> = HashMap::new();
    for i in 0..bricks.len() {
        let mut new_z1 = 1;
        let brick = &mut bricks[i];
        for x in brick.x1..brick.x2+1 {
            for y in brick.y1..brick.y2+1 {
                match floors.get(&(x,y)) {
                    Some((z2,_)) => {
                        if new_z1 < z2 + 1 {
                            new_z1 = z2 + 1;
                        }
                    },
                    _ => {}
                }
            }
        }

        // Update floors and note who supports who
        for x in brick.x1..brick.x2+1 {
            for y in brick.y1..brick.y2+1 {
                match floors.get(&(x,y)) {
                    Some((z2, j)) => {
                        if z2 + 1 == new_z1 {
                            supports.entry(*j).and_modify(|v| { v.insert(i); }).or_insert(HashSet::from([i]));
                            supported_by.entry(i).and_modify(|v| { v.insert(*j); }).or_insert(HashSet::from([*j]));
                        }
                    },
                    _ => {}
                }
                floors.insert((x,y), (brick.z2-(brick.z1-new_z1), i));
            }
        }

        
        brick.z2 = brick.z2-(brick.z1-new_z1);
        brick.z1 = new_z1;
    }

    // Get bricks that can be destroyed
    let mut can_be_removed = 0;
    for i in 0..bricks.len() {
        match supports.get(&i) {
            Some(s) => {
                if s.iter().all(|elem| supported_by.get(elem).unwrap().len() > 1) {
                    can_be_removed += 1;
                }
            },
            None => {
                can_be_removed += 1;
            }
        }

    }

    println!("Part1: {:?}", can_be_removed);
}

fn part2(contents: &str) {

    // Parse input
    let mut bricks = Vec::new();
    for line in contents.lines() {
        let parsed = line.split('~').map(|s| s.split(',').map(|i| i.parse::<u64>().unwrap()).collect::<Vec<u64>>()).collect::<Vec<Vec<u64>>>();
        bricks.push(Brick {
            x1 : parsed[0][0],
            y1 : parsed[0][1],
            z1 : parsed[0][2],
            x2 : parsed[1][0],
            y2 : parsed[1][1],
            z2 : parsed[1][2],
        })
    }

    // Make bricks fall
    bricks.sort_by(|b1, b2| b1.z1.cmp(&b2.z1));
    let mut floors = HashMap::new();
    for i in 0..bricks.len() {
        let mut new_z1 = 1;
        let brick = &mut bricks[i];
        for x in brick.x1..brick.x2+1 {
            for y in brick.y1..brick.y2+1 {
                match floors.get(&(x,y)) {
                    Some((z2,_)) => {
                        if new_z1 < z2 + 1 {
                            new_z1 = z2 + 1;
                        }
                    },
                    _ => {}
                }
            }
        }

        // Update floors and note who supports who
        for x in brick.x1..brick.x2+1 {
            for y in brick.y1..brick.y2+1 {
                floors.insert((x,y), (brick.z2-(brick.z1-new_z1), i));
            }
        }

        
        brick.z2 = brick.z2-(brick.z1-new_z1);
        brick.z1 = new_z1;
    }

    // Get bricks that can be destroyed
    let mut bricks_that_will_fall = 0;
    for i in 0..bricks.len() {
        bricks_that_will_fall += count_bricks_falling(bricks.clone(), i);
    }

    println!("Part2: {:?}", bricks_that_will_fall);
}

#[derive(Debug, Clone)]
struct Brick {
    x1 : u64,
    x2 : u64,
    y1 : u64,
    y2 : u64,
    z1 : u64,
    z2 : u64
}

fn count_bricks_falling(mut bricks : Vec<Brick>, brick_idx : usize) -> u64 {
    let mut result = 0;
    let mut floors = HashMap::new();
    for i in 0..bricks.len() {
        if i != brick_idx {
            let mut new_z1 = 1;
            let brick = &mut bricks[i];
            for x in brick.x1..brick.x2+1 {
                for y in brick.y1..brick.y2+1 {
                    match floors.get(&(x,y)) {
                        Some((z2,_)) => {
                            if new_z1 < z2 + 1 {
                                new_z1 = z2 + 1;
                            }
                        },
                        _ => {}
                    }
                }
            }

            for x in brick.x1..brick.x2+1 {
                for y in brick.y1..brick.y2+1 {
                    floors.insert((x,y), (brick.z2-(brick.z1-new_z1), i));
                }
            }
        
            if brick.z1 != new_z1 {
                result += 1;
                brick.z2 = brick.z2-(brick.z1-new_z1);
                brick.z1 = new_z1;
            }
        }
    }

    result
}