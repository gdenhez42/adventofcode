use std::fs;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

#[derive(Debug, Copy, Clone)]
struct Part {
    nb: u32,
    line: usize,
    col_start: usize,
    col_end: usize
}

fn is_symbol(c: char) -> bool {
    !c.is_digit(10) && c != '.'
}

fn get_part_numbers(engine: &Vec<Vec<char>>) -> Vec<Part> {
    let mut vec = Vec::new();
    for (i, line) in engine.iter().enumerate() {
        let mut current_part: Option<Part> = None;
        for (j, c) in line.iter().enumerate() {
            match (c.to_digit(10), current_part) {
                (Some(d), None) => {
                    current_part = Some(Part {
                        nb: d,
                        line: i,
                        col_start: j,
                        col_end: j+1
                    });
                },
                (Some(d), Some(part)) => {
                    current_part = Some(Part {
                        nb: part.nb * 10 + d,
                        col_end: j+1,
                        ..part
                    });
                }
                (None, Some(part)) => {
                    if is_part_number(&engine, &part) {
                        vec.push(part);
                    }
                    current_part = None;
                },
                _ => {}
            }
        }

        match current_part {
            Some(part) => {
                if is_part_number(&engine, &part) {
                    vec.push(part);
                }
            },
            None => {}
        }
    }
    vec
}

fn is_part_number(engine: &Vec<Vec<char>>, part: &Part) -> bool {
    (part.col_start..part.col_end).any(|j| -> bool {
        let mut has_symbol = false;
        if part.line > 0 {
            if is_symbol(engine[part.line-1][j]) {
                has_symbol = true;
            }
            if j > 0 && is_symbol(engine[part.line-1][j-1]) {
                has_symbol = true;
            }
            if j < (engine[part.line].len()-1) && is_symbol(engine[part.line-1][j+1]) {
                has_symbol = true;
            }
        }
        if part.line < (engine.len() - 1) {
            if is_symbol(engine[part.line+1][j]) {
                has_symbol = true;
            }

            if j > 0 && is_symbol(engine[part.line+1][j-1]) {
                has_symbol = true;
            }
            if j < (engine[part.line].len()-1) && is_symbol(engine[part.line+1][j+1]) {
                has_symbol = true;
            }
        }
        if j > 0 && is_symbol(engine[part.line][j-1]) {
            has_symbol = true;
        }
        if j < (engine[part.line].len()-1) && is_symbol(engine[part.line][j+1]) {
            has_symbol = true;
        }
        has_symbol
    })
}


fn part1(contents: &str) {
    let engine : Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect()).collect();
    let parts = get_part_numbers(&engine);
    let sum = parts.iter().fold(0, |acc, x| acc + x.nb);
    println!("{:?}", sum);
}

fn part2(contents: &str) {
    let engine : Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect()).collect();
    let parts = get_part_numbers(&engine);

    let mut sum = 0;
    for (i, line) in engine.iter().enumerate() {
        for (j, c) in line.iter().enumerate() {
            if *c == '*' {
                let mut adjacent_parts = Vec::new();
                for part in &parts {
                    if !(part.col_start > j + 1 ||
                         part.col_end < j ||
                         part.line < i - 1 ||
                         part.line > i + 1)
                    {
                        adjacent_parts.push(part);
                    }
                }

                if adjacent_parts.len() == 2 {
                    sum += adjacent_parts.iter().fold(1, |acc, x| acc*x.nb)
                }
            }
        }
    }

    println!("{:?}", sum)
}