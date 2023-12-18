use std::fs;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let map = contents.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    let mut rows_to_expand = Vec::new();
    let mut column_to_expand = Vec::new();
    let mut galaxies = Vec::new();
    for i in 0..map.len() {
        let mut is_all_dots = true;
        for j in 0..map[i].len() {
            if map[i][j] == '#' {
                galaxies.push((i,j));
                is_all_dots = false;
            }
        }
        if is_all_dots {
            rows_to_expand.push(i);
        }
    }

    for j in 0..map[0].len() {
        let mut is_all_dots = true;
        for i in 0..map.len() {
            if map[i][j] == '#' {
                is_all_dots = false;
            }
        }
        if is_all_dots {
            column_to_expand.push(j);
        }
    }

    let mut result = 0;
    for i in 0..galaxies.len() {
        for j in i..galaxies.len() {
            let g1 = galaxies[i];
            let g2 = galaxies[j];

            let y1 = g1.0.min(g2.0);
            let y2 = g1.0.max(g2.0);
            let x1 = g1.1.min(g2.1);
            let x2 = g1.1.max(g2.1);

            result += y2 - y1 + rows_to_expand.iter().filter(|&&y| y > y1 && y < y2).count();
            result += x2 - x1 + column_to_expand.iter().filter(|&&x| x > x1 && x < x2).count();
        }
    }

    println!("Part1: {:?}", result);
}

fn part2(contents: &str) {
    let map = contents.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    let mut rows_to_expand = Vec::new();
    let mut column_to_expand = Vec::new();
    let mut galaxies = Vec::new();
    for i in 0..map.len() {
        let mut is_all_dots = true;
        for j in 0..map[i].len() {
            if map[i][j] == '#' {
                galaxies.push((i,j));
                is_all_dots = false;
            }
        }
        if is_all_dots {
            rows_to_expand.push(i);
        }
    }

    for j in 0..map[0].len() {
        let mut is_all_dots = true;
        for i in 0..map.len() {
            if map[i][j] == '#' {
                is_all_dots = false;
            }
        }
        if is_all_dots {
            column_to_expand.push(j);
        }
    }

    let mut result = 0;
    for i in 0..galaxies.len() {
        for j in i..galaxies.len() {
            let g1 = galaxies[i];
            let g2 = galaxies[j];

            let y1 = g1.0.min(g2.0);
            let y2 = g1.0.max(g2.0);
            let x1 = g1.1.min(g2.1);
            let x2 = g1.1.max(g2.1);

            result += y2 - y1 + (1000000-1)*rows_to_expand.iter().filter(|&&y| y > y1 && y < y2).count();
            result += x2 - x1 + (1000000-1)*column_to_expand.iter().filter(|&&x| x > x1 && x < x2).count();
        }
    }
    println!("Part2: {:?}", result);
}