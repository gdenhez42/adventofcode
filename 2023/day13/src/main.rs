use std::fs;

fn get_vertical_reflection(pattern : &Vec<Vec<char>>) -> Option<usize> {
    let mut result = None;
    let nb_col = pattern[0].len();
    for col_idx in 0..(nb_col-1) {
        let mut j = 0;
        let mut is_reflection = true;
        while j <= col_idx && j < nb_col - 1 - col_idx {
            let col1 = pattern.iter().map(|x| x[col_idx-j]).collect::<Vec<char>>();
            let col2 = pattern.iter().map(|x| x[col_idx+1+j]).collect::<Vec<char>>();
            if col1 != col2 {
                is_reflection = false;
            }
            j += 1;
        }
        if is_reflection {
            result = Some(col_idx + 1);
        }
    }
    result
}

fn get_horizontal_reflection(pattern : &Vec<Vec<char>>) -> Option<usize> {
    let mut result = None;
    let nb_lines = pattern.len();
    for line_idx in 0..(nb_lines-1) {
        let mut j = 0;
        let mut is_reflection = true;
        while j <= line_idx && j < nb_lines - 1 - line_idx {
            let line1 = &pattern[line_idx-j];
            let line2 = &pattern[line_idx+1+j];
            if line1 != line2 {
                is_reflection = false;
            }
            j += 1;
        }
        if is_reflection {
            result = Some(line_idx + 1);
        }
    }
    result
}

fn get_vertical_reflection_with_smudge(pattern : &Vec<Vec<char>>) -> Option<usize> {
    let mut result = None;
    let nb_col = pattern[0].len();
    for col_idx in 0..(nb_col-1) {
        let mut j = 0;
        let mut nb_diff = 0;
        while j <= col_idx && j < nb_col - 1 - col_idx {
            let col1 = pattern.iter().map(|x| x[col_idx-j]);
            let col2 = pattern.iter().map(|x| x[col_idx+1+j]);
            for (c1, c2) in col1.zip(col2) {
                if c1 != c2 {
                    nb_diff += 1;
                }
            }
            j += 1;
        }
        if nb_diff == 1 {
            result = Some(col_idx + 1);
        }
    }
    result
}

fn get_horizontal_reflection_with_smudge(pattern : &Vec<Vec<char>>) -> Option<usize> {
    let mut result = None;
    let nb_lines = pattern.len();
    for line_idx in 0..(nb_lines-1) {
        let mut j = 0;
        let mut nb_diff = 0;
        while j <= line_idx && j < nb_lines - 1 - line_idx {
            let line1 = &pattern[line_idx-j];
            let line2 = &pattern[line_idx+1+j];
            for (l1, l2) in line1.iter().zip(line2) {
                if l1 != l2 {
                    nb_diff += 1;
                }
            }
            j += 1;
        }
        if nb_diff == 1 {
            result = Some(line_idx + 1);
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
    for block in contents.split("\n\n") {
        let pattern = block.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();
        match get_horizontal_reflection(&pattern) {
            Some(x) => {
                result += 100*x;
            },
            _ => {
                let x = get_vertical_reflection(&pattern).unwrap();
                result += x;
            }
        }
    }
    println!("Part1: {:?}", result);
}

fn part2(contents: &str) {
    let mut result = 0;
    for block in contents.split("\n\n") {
        let pattern = block.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();
        match get_horizontal_reflection_with_smudge(&pattern) {
            Some(x) => {
                result += 100*x;
            },
            _ => {
                let x = get_vertical_reflection_with_smudge(&pattern).unwrap();
                result += x;
            }
        }
    }
    println!("Part2: {:?}", result);
}
