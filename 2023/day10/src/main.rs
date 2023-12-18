use std::fs;
use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
struct Point(usize, usize);

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let map = contents.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    // Find starting position
    let mut starting_pos_opt = None;
    for (i, line) in map.iter().enumerate() {
        for (j, c) in line.iter().enumerate() {
            if *c == 'S' {
                starting_pos_opt = Some(Point(i,j));
            }
        }
    }
    let starting_pos = starting_pos_opt.unwrap();

    // Find pipes connected to the start
    let mut available_moves = Vec::new();
    if starting_pos.0 > 0 {
        let north_tile = map[starting_pos.0 - 1][starting_pos.1];
        if north_tile == '|' || north_tile == '7' || north_tile == 'F' {
            available_moves.push(Point(starting_pos.0 - 1, starting_pos.1));
        }
    }
    if starting_pos.0 < map.len() - 1 {
        let south_tile = map[starting_pos.0 + 1][starting_pos.1];
        if south_tile == '|' || south_tile == 'J' || south_tile == 'L' {
            available_moves.push(Point(starting_pos.0 + 1, starting_pos.1));
        }
    }
    if starting_pos.1 > 0 {
        let west_tile = map[starting_pos.0][starting_pos.1 - 1];
        if west_tile == '-' || west_tile == 'L' || west_tile == 'F' {
            available_moves.push(Point(starting_pos.0, starting_pos.1 - 1));
        }
    }
    if starting_pos.1 < map[starting_pos.0].len() - 1 {
        let east_tile = map[starting_pos.0][starting_pos.1 + 1];
        if east_tile == '-' || east_tile == 'J' || east_tile == '7' {
            available_moves.push(Point(starting_pos.0, starting_pos.1 + 1));
        }
    }

    // Travel the loop
    let mut last_tile = starting_pos;
    let mut current_tile = available_moves[0];
    let mut step = 1;
    while current_tile != starting_pos {
        match map[current_tile.0][current_tile.1] {
            '|' => {
                if last_tile != Point(current_tile.0 - 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 - 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 + 1, current_tile.1);
                }
            },
            '-' => {
                if last_tile != Point(current_tile.0, current_tile.1 - 1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 - 1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 + 1);
                }
            },
            'L' => {
                if last_tile != Point(current_tile.0 - 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 - 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 + 1);
                }
            },
            'J' => {
                if last_tile != Point(current_tile.0 - 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 - 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 - 1);
                }
            },
            '7' => {
                if last_tile != Point(current_tile.0 + 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 + 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 - 1);
                }
            },
            'F' => {
                if last_tile != Point(current_tile.0 + 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 + 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 + 1);
                }
            },
            _ => {
                panic!("Current tile is not a pipe and this should not happen");
            }
        }
        step += 1;
        
    }

    println!("Part1: {:?}", step/2);
}

fn part2(contents: &str) {
    let map = contents.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    // Find starting position
    let mut starting_pos_opt = None;
    for (i, line) in map.iter().enumerate() {
        for (j, c) in line.iter().enumerate() {
            if *c == 'S' {
                starting_pos_opt = Some(Point(i,j));
            }
        }
    }
    let starting_pos = starting_pos_opt.unwrap();

    // Find pipes connected to the start
    let mut available_moves = Vec::new();
    if starting_pos.0 > 0 {
        let north_tile = map[starting_pos.0 - 1][starting_pos.1];
        if north_tile == '|' || north_tile == '7' || north_tile == 'F' {
            available_moves.push(Point(starting_pos.0 - 1, starting_pos.1));
        }
    }
    if starting_pos.0 < map.len() - 1 {
        let south_tile = map[starting_pos.0 + 1][starting_pos.1];
        if south_tile == '|' || south_tile == 'J' || south_tile == 'L' {
            available_moves.push(Point(starting_pos.0 + 1, starting_pos.1));
        }
    }
    if starting_pos.1 > 0 {
        let west_tile = map[starting_pos.0][starting_pos.1 - 1];
        if west_tile == '-' || west_tile == 'L' || west_tile == 'F' {
            available_moves.push(Point(starting_pos.0, starting_pos.1 - 1));
        }
    }
    if starting_pos.1 < map[starting_pos.0].len() - 1 {
        let east_tile = map[starting_pos.0][starting_pos.1 + 1];
        if east_tile == '-' || east_tile == 'J' || east_tile == '7' {
            available_moves.push(Point(starting_pos.0, starting_pos.1 + 1));
        }
    }

    // Find loop tiles
    let mut last_tile = starting_pos;
    let mut current_tile = available_moves[0];
    let mut loop_pipes = HashSet::new();
    loop_pipes.insert(current_tile);
    while current_tile != starting_pos {
        match map[current_tile.0][current_tile.1] {
            '|' => {
                if last_tile != Point(current_tile.0 - 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 - 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 + 1, current_tile.1);
                }
            },
            '-' => {
                if last_tile != Point(current_tile.0, current_tile.1 - 1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 - 1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 + 1);
                }
            },
            'L' => {
                if last_tile != Point(current_tile.0 - 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 - 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 + 1);
                }
            },
            'J' => {
                if last_tile != Point(current_tile.0 - 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 - 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 - 1);
                }
            },
            '7' => {
                if last_tile != Point(current_tile.0 + 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 + 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 - 1);
                }
            },
            'F' => {
                if last_tile != Point(current_tile.0 + 1, current_tile.1) {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0 + 1, current_tile.1);
                } else {
                    last_tile = current_tile;
                    current_tile = Point(current_tile.0, current_tile.1 + 1);
                }
            },
            _ => {
                panic!("Current tile is not a pipe and this should not happen");
            }
        }
        loop_pipes.insert(current_tile);   
    }

    // Find tiles inside loop
    let mut nb = 0;
    let mut last_pipe = None;
    for i in 0..map.len() {
        let mut nb_loop_pipes = 0;
        for j in 0..map[i].len() {
            if loop_pipes.contains(&Point(i,j)) {
                match (last_pipe, map[i][j]) {
                    (_, '-') => {},
                    (Some('L'), '7') => {},
                    (Some('F'), 'J') => {},
                    (_, p) => {
                        last_pipe = Some(p);
                        nb_loop_pipes += 1;
                    }
                }
            } else if nb_loop_pipes % 2 == 1 {
                nb += 1;
            }
        }
    }


    println!("Part2: {:?}", nb);
}