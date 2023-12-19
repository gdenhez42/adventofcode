use std::fs;
use std::collections::HashMap;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let map = contents.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    let mut result = 0;
    for j in 0..map[0].len() {
        let mut tilt_to = 0;
        for i in 0..map.len() {
            match map[i][j] {
                '#' => {
                    tilt_to = i + 1;
                },
                'O' => {
                    result += map.len() - tilt_to;
                    tilt_to += 1;
                }
                _ => {}
            }
        }
    }

    println!("Part1: {:?}", result);
}

fn part2(contents: &str) {
    let mut map = contents.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    let mut i = 0;
    let mut h = HashMap::new();
    while h.get(&map).is_none() {
        h.insert(map.clone(), i);
        tilt_north(&mut map);
        tilt_west(&mut map);
        tilt_south(&mut map);
        tilt_east(&mut map);
        i += 1;
    }

    let map_idx = h.get(&map).unwrap() + ((1000000000 - h.get(&map).unwrap()) % (i - h.get(&map).unwrap()));

    for (m, idx) in &h {
        if *idx == map_idx {
            println!("Part2: {:?}", total_load(m));
        }
    }
}

fn total_load(map : &Vec<Vec<char>>) -> usize {
    let mut result = 0;
    for i in 0..map.len() {
        for j in 0..map[i].len() {
            if map[i][j] == 'O' {
                result += map.len() - i;
            }
        }
    }
    result
}

fn tilt_north(map : &mut Vec<Vec<char>>) {
    for j in 0..map[0].len() {
        let mut tilt_to = 0;
        for i in 0..map.len() {
            match map[i][j] {
                '#' => {
                    tilt_to = i + 1;
                },
                'O' => {
                    map[i][j] = '.';
                    map[tilt_to][j] = 'O';
                    tilt_to += 1;
                }
                _ => {}
            }
        }
    }
}

fn tilt_south(map : &mut Vec<Vec<char>>) {
    let max = map.len() - 1;
    for j in 0..map[0].len() {
        let mut tilt_to = max + 1;
        for i in 0..map.len() {
            match map[max - i][j] {
                '#' => {
                    // note to myself: rust panics if there is an underflow
                    tilt_to = max-i; 
                },
                'O' => {
                    tilt_to -= 1;
                    map[max-i][j] = '.';
                    map[tilt_to][j] = 'O';
                }
                _ => {}
            }
        }
    }
}

fn tilt_west(map : &mut Vec<Vec<char>>) {
    for i in 0..map.len() {
        let mut tilt_to = 0;
        for j in 0..map[i].len() {
            match map[i][j] {
                '#' => {
                    tilt_to = j + 1;
                },
                'O' => {
                    map[i][j] = '.';
                    map[i][tilt_to] = 'O';
                    tilt_to += 1;
                }
                _ => {}
            }
        }
    }
}

fn tilt_east(map : &mut Vec<Vec<char>>) {
    let max = map[0].len() - 1;
    for i in 0..map.len() {
        let mut tilt_to = max + 1;
        for j in 0..map[i].len() {
            match map[i][max-j] {
                '#' => {
                    tilt_to = max-j;
                },
                'O' => {
                    tilt_to -= 1;
                    map[i][max-j] = '.';
                    map[i][tilt_to] = 'O';
                }
                _ => {}
            }
        }
    }
}