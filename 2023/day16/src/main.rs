use std::fs;
use std::collections::HashSet;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let map = contents.lines().map(|l| l.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    println!("Part1: {:?}", count_energized(&map, Beam {
        x: 0,
        y: 0,
        dir: Dir::Right
    }));
}

fn part2(contents: &str) {
    let map = contents.lines().map(|l| l.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    let mut max = 0;
    for y in 0..map.len() {
        max = max.max(count_energized(&map, Beam {
            x: 0,
            y: y,
            dir: Dir::Right
        }));
        max = max.max(count_energized(&map, Beam {
            x: map[y].len() - 1,
            y: y,
            dir: Dir::Left
        }));
    }

    for x in 0..map[0].len() {
        max = max.max(count_energized(&map, Beam {
            x: x,
            y: 0,
            dir: Dir::Down
        }));
        max = max.max(count_energized(&map, Beam {
            x: x,
            y: map.len()-1,
            dir: Dir::Up
        }));
    }

    println!("Part2: {:?}", max);
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
enum Dir {
    Up,
    Down,
    Left,
    Right
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct Beam {
    x: usize,
    y: usize,
    dir: Dir
}

fn count_energized(map : &Vec<Vec<char>>, starting_beam : Beam) -> usize {
    let mut beams = HashSet::new();
    beams.insert(starting_beam);
    let mut visits = HashSet::<Beam>::new();

    while beams.len() > 0 {
        let mut temp = HashSet::new();
        for b in beams {
            if visits.insert(b.clone()) {
                let Beam {x, y, dir} = b;
                match (map[y][x], dir) {
                    ('.', Dir::Right) if x < map[y].len() - 1 => {
                        temp.insert(Beam {x: x+1, y: y, dir: Dir::Right});
                    },
                    ('.', Dir::Left) if x > 0 => {
                        temp.insert(Beam {x: x-1, y: y, dir: Dir::Left});
                    },
                    ('.', Dir::Up) if y > 0 => {
                        temp.insert(Beam {x: x, y: y-1, dir: Dir::Up});
                    },
                    ('.', Dir::Down) if y < map.len() - 1 => {
                        temp.insert(Beam {x: x, y: y+1, dir: Dir::Down});
                    },
                    ('/', Dir::Right) if y > 0 => {
                        temp.insert(Beam {x: x, y: y-1, dir: Dir::Up});
                    },
                    ('/', Dir::Left) if y < map.len() - 1 => {
                        temp.insert(Beam {x: x, y: y+1, dir: Dir::Down});
                    },
                    ('/', Dir::Up) if x < map[y].len() - 1 => {
                        temp.insert(Beam {x: x+1, y: y, dir: Dir::Right});
                    },
                    ('/', Dir::Down) if x > 0 => {
                        temp.insert(Beam {x: x-1, y: y, dir: Dir::Left});
                    },
                    ('\\', Dir::Right) if y < map.len() - 1 => {
                        temp.insert(Beam {x: x, y: y+1, dir: Dir::Down});
                    },
                    ('\\', Dir::Left) if y > 0 => {
                        temp.insert(Beam {x: x, y: y-1, dir: Dir::Up});
                    },
                    ('\\', Dir::Up) if x > 0 => {
                        temp.insert(Beam {x: x-1, y: y, dir: Dir::Left});
                    },
                    ('\\', Dir::Down) if x < map[y].len() - 1 => {
                        temp.insert(Beam {x: x+1, y: y, dir: Dir::Right});
                    },
                    ('|', Dir::Right) | ('|', Dir::Left) => {
                        if y < map.len() - 1 {
                            temp.insert(Beam {x: x, y: y+1, dir: Dir::Down});
                        }
                        if y > 0 {
                            temp.insert(Beam {x: x, y: y-1, dir: Dir::Up});
                        }
                    },
                    ('|', Dir::Up) if y > 0 => {
                        temp.insert(Beam {x: x, y: y-1, dir: Dir::Up});
                    },
                    ('|', Dir::Down) if y < map.len() - 1 => {
                        temp.insert(Beam {x: x, y: y+1, dir: Dir::Down});
                    },
                    ('-', Dir::Up) | ('-', Dir::Down) => {
                        if x < map[y].len() - 1 {
                            temp.insert(Beam {x: x+1, y: y, dir: Dir::Right});
                        }
                        if x > 0 {
                            temp.insert(Beam {x: x-1, y: y, dir: Dir::Left});
                        }
                    },
                    ('-', Dir::Right) if x < map[y].len() - 1 => {
                        temp.insert(Beam {x: x+1, y: y, dir: Dir::Right});
                    },
                    ('-', Dir::Left) if x > 0 => {
                        temp.insert(Beam {x: x-1, y: y, dir: Dir::Left});
                    },
                    _ => {}
                }
            }
        }
        beams = temp;
    }

    let mut energized = HashSet::new();
    for v in visits {
        energized.insert((v.x, v.y));
    }

    energized.len()
}