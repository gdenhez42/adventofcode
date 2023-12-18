use std::fs;

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
        let mut prediction = Vec::new();
        prediction.push(line.split_whitespace().map(|x| x.parse::<i64>().unwrap()).collect::<Vec<i64>>());
        let mut last_step = prediction.last().unwrap();
        while last_step.iter().any(|x| *x != 0) {
            let mut step = Vec::new();
            for i in 1..last_step.len() {
                step.push(last_step[i] - last_step[i-1]);
            }
            prediction.push(step);
            last_step = prediction.last().unwrap();
        }
        let mut to_add = 0;
        for i in 1..prediction.len() {
            let idx = prediction.len() - 1 - i;
            to_add = prediction[idx].last().unwrap() + to_add;
        }
        result += to_add;
    }

    println!("Part1: {:?}", result);
}

fn part2(contents: &str) {
    let mut result = 0;
    for line in contents.lines() {
        let mut prediction = Vec::new();
        prediction.push(line.split_whitespace().map(|x| x.parse::<i64>().unwrap()).collect::<Vec<i64>>());
        let mut last_step = prediction.last().unwrap();
        while last_step.iter().any(|x| *x != 0) {
            let mut step = Vec::new();
            for i in 1..last_step.len() {
                step.push(last_step[i] - last_step[i-1]);
            }
            prediction.push(step);
            last_step = prediction.last().unwrap();
        }
        let mut to_remove = 0;
        for i in 1..prediction.len() {
            let idx = prediction.len() - 1 - i;
            to_remove = prediction[idx].first().unwrap() - to_remove;
        }
        result += to_remove;
    }

    println!("Part2: {:?}", result);
}