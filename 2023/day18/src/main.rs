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

    let mut dig_plan = Vec::new();
    for line in contents.lines() {
        let [dir, nb_str, _] = line.split_whitespace().collect::<Vec<&str>>()[..] else { panic!("Unexpected input!") };
        let nb  = nb_str.parse::<i32>().unwrap();
        dig_plan.push((dir, nb));
    }

    let mut current = (0, 0);
    let mut border = HashMap::new();
    for i in 0..dig_plan.len() {
        let previous_plan = dig_plan[if i > 0 { i-1 } else {dig_plan.len() - 1}];
        let current_plan = dig_plan[i];
        
        match (previous_plan.0, current_plan.0) {
            ("U", "R") => {
                for _ in 0..current_plan.1 {
                    border.insert((current.0, current.1), "U");
                    current = (current.0 + 1, current.1);
                }
            },
            ("U", "L") => {
                for _ in 0..current_plan.1 {
                    border.insert((current.0, current.1), "U");
                    current = (current.0 - 1, current.1);
                }
            },
            ("D", "R") => {
                for _ in 0..current_plan.1 {
                    border.insert((current.0, current.1), "D");
                    current = (current.0 + 1, current.1);
                }
            },
            ("D", "L") => {
                for _ in 0..current_plan.1 {
                    border.insert((current.0, current.1), "D");
                    current = (current.0 - 1, current.1);
                }
            },
            (_, "U") => {
                for _ in 0..current_plan.1 {
                    border.insert((current.0, current.1), "U");
                    current = (current.0, current.1 - 1);
                }
            },
            (_, "D") => {
                for _ in 0..current_plan.1 {
                    border.insert((current.0, current.1), "D");
                    current = (current.0, current.1 + 1);
                }
            },
            _ => {
                panic!("Unexpected input");
            }
        }
    }

    let mut min_x = 0;
    let mut max_x = 0;
    let mut min_y = 0;
    let mut max_y = 0;

    for ((x, y),_) in &border {
        if *x < min_x {
            min_x = *x;
        }
        if *y < min_y {
            min_y = *y;
        }
        if *x > max_x {
            max_x = *x;
        }
        if *y > max_y {
            max_y = *y;
        }
    }

    let mut area = 0;
    for y in min_y..max_y+1 {
        let mut last_border = None;
        let mut inside = false;
        for x in min_x..max_x+1 {
            match (last_border, border.get(&(x,y))) {
                (Some("U"), Some(&"U")) => {
                    area += 1;
                },
                (Some("D"), Some(&"D")) => {
                    area += 1;
                },
                (_, Some(&"U")) => {
                    inside = !inside;
                    last_border = Some("U");
                    area += 1;
                },
                (_, Some(&"D")) => {
                    inside = !inside;
                    last_border = Some("D");
                    area += 1;
                },
                (_, None) => {
                    if inside {
                        area += 1;
                    }
                },
                _ => {}
            }
        }
    }

    println!("Part1: {:?}", area);
}

fn part2(_contents: &str) {

    println!("Part2: {:?}", 42);
}
