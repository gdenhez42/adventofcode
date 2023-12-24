use std::fs;
use std::collections::HashSet;
use std::collections::HashMap;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let map = contents.lines().map(|l| l.chars().map(|c| (c as usize) - 48).collect::<Vec<usize>>()).collect::<Vec<Vec<usize>>>();
    let h = map.len();
    let w = map[0].len();

    // Good old Dijkstra
    let mut distances = HashMap::new();
    let mut visited = HashSet::<Node>::new();

    // First iteration
    let current_node_1 = Node {
        x: 0,
        y: 0,
        dir: Dir::V
    };
    for (n, d) in get_neighbors(&current_node_1, &map, &visited) {
        distances.insert(n, d);
    }
    let current_node_2 = Node {
        x: 0,
        y: 0,
        dir: Dir::H
    };
    for (n, d) in get_neighbors(&current_node_2, &map, &visited) {
        distances.insert(n, d);
    }
    let mut min_d = None;
    let mut min_n = None;
    for (n, d) in &distances {
        match min_d {
            None => {
                min_d = Some(*d);
                min_n = Some(n.clone());
            },
            Some(m) => {
                if *d < m {
                    min_d = Some(*d);
                    min_n = Some(n.clone());
                }
            }
        }
    }
    let mut current_node = min_n.unwrap();
    let mut distance = min_d.unwrap();

    while current_node.x != w-1 || current_node.y != h-1 {
        // Visited
        visited.insert(current_node.clone());
        distances.remove(&current_node);

        // New distances
        for (n, delta) in get_neighbors(&current_node, &map, &visited) {
            let new_d = distance + delta;
            match distances.get(&n) {
                Some(d) => {
                    if new_d < *d {
                        distances.insert(n, new_d);
                    }
                },
                None => {
                    distances.insert(n, new_d);
                }
            }
        }
        
        let mut min = None;
        for (n, d) in &distances {
            match min {
                None => {
                    min = Some(*d);
                    current_node = n.clone();
                },
                Some(m) => {
                    if *d < m {
                        min = Some(*d);
                        current_node = n.clone();
                    }
                }
            }
        }
        distance = min.unwrap();
    }

    println!("Part1: {:?}", (current_node, distance));
}

fn part2(_contents: &str) {
    println!("Part2: {:?}", 42);
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
enum Dir {
    V,
    H
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct Node {
    x: usize,
    y: usize,
    dir: Dir
}

fn get_neighbors(node: &Node, map: &Vec<Vec<usize>>, visited: &HashSet::<Node>) -> Vec<(Node, usize)> {
    let mut result = Vec::new();

    if node.dir == Dir::V {
        let mut rev = 0;
        let mut forw = 0;

        for i in 1..4 {
            if node.x >= i {
                rev += map[node.y][node.x - i];
                let node = Node {
                    x: node.x - i,
                    y: node.y,
                    dir: Dir::H
                };
                if !visited.contains(&node) {
                    result.push((node, rev));
                }
            }
            if node.x + i < map[0].len() {
                forw += map[node.y][node.x + i];
                let node = Node {
                    x: node.x + i,
                    y: node.y,
                    dir: Dir::H
                };
                if !visited.contains(&node) {
                    result.push((node, forw));
                }
            }
        }

    }

    if node.dir == Dir::H {
        let mut rev = 0;
        let mut forw = 0;

        for i in 1..4 {
            if node.y >= i {
                rev += map[node.y - i][node.x];
                let node = Node {
                    x: node.x,
                    y: node.y - i,
                    dir: Dir::V
                };
                if !visited.contains(&node) {
                    result.push((node, rev));
                }
            }
            if node.y + i < map.len() {
                forw += map[node.y + i][node.x];
                let node = Node {
                    x: node.x,
                    y: node.y + i,
                    dir: Dir::V
                };
                if !visited.contains(&node) {
                    result.push((node, forw));
                }
            }
        }

    }

    result
}
