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

fn part2(contents: &str) {
    let mut dig_plan = Vec::new();
    for line in contents.lines() {
        let [_, _, hex_code] = line.split_whitespace().collect::<Vec<&str>>()[..] else { panic!("Unexpected input!") };
        let nb  = i64::from_str_radix(&hex_code[2..7], 16).unwrap();
        let dir = hex_code[7..8].parse::<i64>().unwrap();
        dig_plan.push((dir, nb));
    }

    // Find borders and vertical lines
    let mut current = (0, 0);
    let mut border = Vec::new();
    let mut vertical_lines = Vec::new();
    for i in 0..dig_plan.len() {
        let previous_plan = dig_plan[if i > 0 { i-1 } else {dig_plan.len() - 1}];
        let current_plan = dig_plan[i];
        
        // 0 : Right, 1: Down, 2: Left, 3: Up
        match (previous_plan.0, current_plan.0) {
            (0, 1) => {
                border.push((current.0, current.1, 1));
                vertical_lines.push((current.0, current.1, current.1 + current_plan.1));
                current = (current.0, current.1 + current_plan.1);
            },
            (0, 3) => {
                border.push((current.0, current.1, 3));
                vertical_lines.push((current.0, current.1 - current_plan.1, current.1));
                current = (current.0, current.1 - current_plan.1);
            },
            (1, 0) => {
                border.push((current.0, current.1, 1));
                current = (current.0 + current_plan.1, current.1);
            },
            (1, 2) => {
                border.push((current.0, current.1, 1));
                current = (current.0 - current_plan.1, current.1);
            },
            (2, 1) => {
                border.push((current.0, current.1, 1));
                vertical_lines.push((current.0, current.1, current.1 + current_plan.1));
                current = (current.0, current.1 + current_plan.1);
            },
            (2, 3) => {
                border.push((current.0, current.1, 3));
                vertical_lines.push((current.0, current.1 - current_plan.1, current.1));
                current = (current.0, current.1 - current_plan.1);
            },
            (3, 0) => {
                border.push((current.0, current.1, 3));
                current = (current.0 + current_plan.1, current.1);
            },
            (3, 2) => {
                border.push((current.0, current.1, 3));
                current = (current.0 - current_plan.1, current.1);
            },
            _ => {
                panic!("Unexpected input");
            }
        }
    }

    border.sort_by(|a, b| a.1.cmp(&b.1));
    vertical_lines.sort_by(|a, b| a.1.cmp(&b.1));

    // Find min and max
    let mut min_x = 0;
    let mut max_x = 0;
    let mut min_y = 0;
    let mut max_y = 0;

    for (x, y, _) in &border {
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

    // Calculate volume
    let mut result : i64 = 0;
    let mut y = min_y;
    let mut border_idx = 0;
    loop {
        let mut border_y = Vec::new();
        let mut vertical_lines_y = Vec::new();
        let new_y;
        loop {
            if border[border_idx].1 > y {
                new_y = border[border_idx].1;
                break;
            }

            border_y.push((border[border_idx].0, border[border_idx].2));
            border_idx += 1;
        }

        for v in &vertical_lines {
            if v.1 < y && v.2 > y {
                vertical_lines_y.push(v.0);
            }
        }

        border_y.sort_by(|a, b| a.0.cmp(&b.0));
        vertical_lines_y.sort();

        // Get border size
        let mut inside = false;
        let mut border_size = 0;
        let mut i = 0;
        let mut j = 0;
        let mut x = min_x;
        loop {
            if i == border_y.len() && j == vertical_lines_y.len() {
                break;
            }
            if j == vertical_lines_y.len() || (i < border_y.len() && border_y[i].0 < vertical_lines_y[j]) {
                let (x1, dir1) = border_y[i];
                let (x2, dir2) = border_y[i+1];
                if inside {
                    border_size += x1 - x;
                }
                border_size += x2-x1+1;
                
                // 1: Down, 3: Up
                match (dir1, dir2) {
                    (1, 1) => {
                        inside = !inside;
                    },
                    (3, 3) => {
                        inside = !inside;
                    },
                    _ => {
                    }
                }

                x = x2 + 1;
                i += 2;
            } else {
                let x1 = vertical_lines_y[j];
                if inside {
                    border_size += x1 - x;
                }
                border_size += 1;
                inside = !inside;
                x = x1 + 1;
                j += 1;
            }

        }

        println!("y: {:?}, Border size: {:?}", y, border_size);

        result += border_size;

        // Get rectangle size
        if new_y > y + 1 {
            y += 1;
            vertical_lines_y = Vec::new();
            for v in &vertical_lines {
                if v.1 < y && v.2 > y {
                    vertical_lines_y.push(v.0);
                }
            }
            vertical_lines_y.sort();

            x = min_x;
            inside = false;
            let mut rect_width = 0;
            for v in vertical_lines_y {
                if inside {
                    rect_width += v - x;
                }
                rect_width += 1;
                inside = !inside;
                x = v + 1;
            }
            result += rect_width*(new_y - y);
            println!("y: {:?}-{:?}, rect_width: {:?}", y, new_y, rect_width);
        }
        // 112 010
        // 

        y = new_y;
        if y == max_y {
            break;
        }
    }

    // Last border
    let mut border_y = Vec::new();
    let mut border_size = 0;
    while border_idx < border.len() {
        border_y.push(border[border_idx].0);
        border_y.push(border[border_idx + 1].0);
        border_idx += 2;
    }
    border_y.sort();
    let mut j = 0;
    while j < border_y.len() {
        let x1 = border_y[j];
        let x2 = border_y[j+1];
        border_size += x2 - x1 + 1;
        j += 2;
    }
    println!("y: {:?}, Border size: {:?}", max_y, border_size);
    result += border_size;

    println!("Part2: {:?}", result);
}
