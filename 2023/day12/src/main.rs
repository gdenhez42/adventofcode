use std::fs;

fn possible_places(all_springs : &Vec<char>, nb_damaged : usize, start_idx : usize) -> Vec<usize> {
    let mut i = start_idx;
    let mut possible = Vec::new();
    loop {
        if i > all_springs.len()-nb_damaged {
            break possible
        }

        let mut j = i;
        while j < i + nb_damaged && all_springs[j] != '.' {
            j += 1;
        }
        
        if j == i + nb_damaged {
            if j == all_springs.len() {
                possible.push(i + nb_damaged + 1);
            } else if all_springs[j] == '.' || all_springs[j] == '?' {
                possible.push(i + nb_damaged + 1);
            }
        }

        if all_springs[i] == '#' {
            break possible;
        }

        i += 1;
    }
}

fn unfold_springs(all_springs_folded: &Vec<char>) -> Vec<char> {
    let mut all_springs = Vec::new();
    for _ in 0..4 {
        all_springs.extend(all_springs_folded);
        all_springs.push('?');
    }
    all_springs.extend(all_springs_folded);
    all_springs
}

fn unfold_damage(damaged_folded: &Vec<usize>) -> Vec<usize> {
    let mut damaged = Vec::new();
    for _ in 0..5 {
        damaged.extend(damaged_folded);
    }
    damaged
}

fn merge_possibilities(possible : &Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    let mut result = Vec::new();
    for (times, p) in possible {
        match result.iter().position(|&(_t, x)| x == *p) {
            Some(i) => {
                let (t, _x) = result[i];
                result[i] = (t+*times, *p);
            },
            None => {
                result.push((*times, *p));
            }
        }
    }
    result
}

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let mut result = 0;
    for line in contents.lines() {
        let mut parts = line.split_whitespace();
        let all_springs = parts.next().unwrap().chars().collect::<Vec<char>>();
        let damaged = parts.next().unwrap().split(',').map(|x| x.parse::<usize>().unwrap()).collect::<Vec<usize>>();
        
        let mut possible = Vec::new();
        possible.push(0);
        for d in damaged {
            let mut temp = Vec::new();
            for p in possible {
                temp.extend(possible_places(&all_springs, d, p));
            }
            possible = temp;
        }

        // Eliminate a possibility if the end contains a #
        // while we already placed every damaged group
        let mut temp = Vec::new();
        for p in possible {
            let mut is_possible = true;
            for i in p..all_springs.len() {
                if all_springs[i] == '#' {
                    is_possible = false;
                }
            }
            if is_possible {
                temp.push(p);
            }
        }
        possible = temp;

        result += possible.len();
    }

    println!("Part1: {:?}", result);
}

fn part2(contents: &str) {
    let mut result = 0;
    for line in contents.lines() {
        let mut parts = line.split_whitespace();
        let all_springs_folded = parts.next().unwrap().chars().collect::<Vec<char>>();
        let damaged_folded = parts.next().unwrap().split(',').map(|x| x.parse::<usize>().unwrap()).collect::<Vec<usize>>();

        let all_springs = unfold_springs(&all_springs_folded);
        let damaged = unfold_damage(&damaged_folded);
        
        let mut possible = Vec::new();
        possible.push((1, 0));
        for d in damaged {
            let mut temp = Vec::new();
            for (times, p) in possible {
                temp.extend(possible_places(&all_springs, d, p).iter().map(|&x| (times, x)));
            }
            possible = temp;
            possible = merge_possibilities(&possible);
        }

        let mut temp = Vec::new();
        for (times, p) in possible {
            let mut is_possible = true;
            for i in p..all_springs.len() {
                if all_springs[i] == '#' {
                    is_possible = false;
                }
            }
            if is_possible {
                temp.push((times, p));
            }
        }
        possible = temp;

        result += possible.iter().fold(0, |acc, (t, _)|  acc + t);
    }

    println!("Part2: {:?}", result);
}