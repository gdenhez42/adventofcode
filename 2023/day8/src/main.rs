use std::fs;
use std::collections::HashMap;

#[derive(Debug)]
struct NodeStep {
    step: usize,
    node: String
}

#[derive(Debug, Clone)]
struct Loop {
    loop_start: u64,
    loop_size: u64,
    endings: Vec<u64>
}

fn ends_with(node : &String, ending: char) -> bool {
    let end = node.chars().last().unwrap();
    end == ending
}


fn find_loop(directions: &Vec<char>, nodes : &HashMap<String, (String, String)>, starting_node : &String) -> Loop {
    let mut steps = Vec::<NodeStep>::new();
    let mut current_node = starting_node.clone();
    let mut endings = Vec::new();
    let mut step = 0;
    loop {
        let dir_idx = step % directions.len();
        match steps.iter().position(|x| x.step == dir_idx && x.node == current_node) {
            Some(i) => {
                break Loop {
                    loop_start: i as u64,
                    loop_size: (steps.len() - i) as u64,
                    endings: endings
                }
            }
            _ => {
                steps.push(NodeStep {
                    step: dir_idx,
                    node: current_node.clone()
                });
                if ends_with(&current_node, 'Z') {
                    endings.push(step as u64);
                }
                let direction = directions[dir_idx];
                match direction {
                    'L' => {
                        current_node = nodes.get(&current_node).unwrap().0.clone();
                    }
                    _ => {
                        current_node = nodes.get(&current_node).unwrap().1.clone();
                    }
                }
                step = step + 1;
            }

        }
    }
}

fn merge_loops(l1 : &Loop, l2 : &Loop) -> Loop {
    let loop_start = if l1.loop_start < l2.loop_start { l2.loop_start } else { l1.loop_start };
    let loop_size = smallest_common_multiple(&vec![l1.loop_size, l2.loop_size]);
    
    let mut endings = Vec::new();
    for e1 in &l1.endings {
        for e2 in &l2.endings {
            if *e1 == *e2 {
                endings.push(*e1);
            } else if *e1 > l1.loop_start && *e2 > l2.loop_start {
                let mut in_loop1 = *e1;
                let mut in_loop2 = *e2;
                let max = loop_size + loop_start;

                while in_loop1 != in_loop2 && in_loop1 < max && in_loop2 < max {
                    if in_loop1 < in_loop2 {
                        let diff = in_loop2 - in_loop1;
                        let n = if diff % l1.loop_size == 0 {diff / l1.loop_size} else {diff / l1.loop_size + 1};

                        in_loop1 += l1.loop_size*n;
                    } else {
                        let diff = in_loop1 - in_loop2;
                        let n = if diff % l2.loop_size == 0 {diff / l2.loop_size} else {diff / l2.loop_size + 1};

                        in_loop2 += l2.loop_size*n;
                    }
                }
                if in_loop1 == in_loop2 {
                    endings.push(in_loop1);
                }
            } else if *e1 > l1.loop_start {
                let mut in_loop1 = *e1;
                while in_loop1 < *e2 {
                    in_loop1 += l1.loop_size;
                }
                if in_loop1 == *e2 {
                    endings.push(in_loop1);
                }
            } else if *e2 > l2.loop_start {
                let mut in_loop2 = *e2;
                while in_loop2 < *e1 {
                    in_loop2 += l2.loop_size;
                }
                if in_loop2 == *e1 {
                    endings.push(in_loop2);
                }
            }
        }
    }

    Loop {
        loop_start : loop_start,
        loop_size : loop_size,
        endings : endings
    }
}


fn prime_decomposition(mut nb : u64) -> Vec<u64> {
    let mut primes = Vec::new();
    let sqrt = ((nb as f64).sqrt() as u64) + 1;
    for i in 2..sqrt {
        while nb % i == 0 {
            primes.push(i);
            nb = nb / i;
        }

    }
    if nb > 1 {
        primes.push(nb)
    }
    primes
}

fn merge_primes(p1 : &Vec<u64>, p2 : &Vec<u64>) -> Vec<u64> {
    let mut result = Vec::new();

    let mut i1 = 0;
    let mut i2 = 0;
    while i1 < p1.len() && i2 < p2.len() {
        if p1[i1] == p2[i2] {
            result.push(p1[i1]);
            i1 += 1;
            i2 += 1;
        } else if p1[i1] < p2[i2] {
            result.push(p1[i1]);
            i1 += 1;
        } else {
            result.push(p2[i2]);
            i2 += 1;
        }
    }
    while i1 < p1.len() {
        result.push(p1[i1]);
        i1 += 1;
    }
    while i2 < p2.len() {
        result.push(p2[i2]);
        i2 += 1;
    }

    result
}

fn smallest_common_multiple(nbs : &Vec<u64>) -> u64 {
    let mut primes = Vec::new();
    for nb in nbs {
        let p = prime_decomposition(*nb);
        primes = merge_primes(&p, &primes);
    }
    primes.iter().fold(1, |x, acc| x * acc)
}

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let mut lines = contents.lines();
    let directions = lines.next().unwrap().chars().collect::<Vec<char>>();

    let mut nodes : HashMap<String, (String, String)> = HashMap::new();
    for line in lines.skip(1) {
        let tokens = line.split_whitespace().collect::<Vec<&str>>();
        let start = String::from(tokens[0]);
        let mut left = String::from(tokens[2]);
        left.pop();
        left.remove(0);
        let mut right = String::from(tokens[3]);
        right.pop();


        nodes.insert(start, (left, right));
    }


    let mut current_node = String::from("AAA");
    let mut steps = 0;
    while current_node != String::from("ZZZ") {
        let direction = directions[steps % directions.len()];
        match direction {
            'L' => {
                current_node = nodes.get(&current_node).unwrap().0.clone();
            }
            _ => {
                current_node = nodes.get(&current_node).unwrap().1.clone();
            }
        }
        steps += 1;
    }


    println!("Part1 {:?}", steps);
}



fn part2(contents: &str) {
    let mut lines = contents.lines();
    let directions = lines.next().unwrap().chars().collect::<Vec<char>>();

    let mut nodes : HashMap<String, (String, String)> = HashMap::new();
    for line in lines.skip(1) {
        let tokens = line.split_whitespace().collect::<Vec<&str>>();
        let start = String::from(tokens[0]);
        let mut left = String::from(tokens[2]);
        left.pop();
        left.remove(0);
        let mut right = String::from(tokens[3]);
        right.pop();


        nodes.insert(start, (left, right));
    }

    let mut starting_nodes = Vec::<String>::new();
    for node in nodes.keys() {
        if ends_with(node, 'A') {
            starting_nodes.push(node.clone())
        }
    }

    let looops = starting_nodes.iter().map(|x| find_loop(&directions, &nodes, &x)).collect::<Vec<Loop>>();
    let merged = looops.iter().skip(1).fold(looops[0].clone(), |x, acc| -> Loop {
        merge_loops(&x, &acc)
    });

    println!("Part2: {:?}", merged.endings.iter().min().unwrap());
}