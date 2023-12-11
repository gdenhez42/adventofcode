use std::fs;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let mut count = 0;
    for line in contents.split("\n") {
        let mut first_digit = None;
        let mut last_digit = None;

        for c in line.chars() {
            if c.is_digit(10) {
                if first_digit.is_none() {
                    first_digit = Some(c)
                }
                last_digit = Some(c)
            }
            
        }

        count += first_digit.unwrap().to_digit(10).unwrap() * 10 + last_digit.unwrap().to_digit(10).unwrap()
    }

    println!("Part1: {:?}", count)
}

fn part2(contents: &str) {
    let mut count = 0;
    for line in contents.lines() {
        let mut first_digit = None;
        let mut last_digit = None;

        for i in 0..line.len() {
            let sliced = &line[i..];
            if sliced.starts_with("one") || sliced.starts_with("1") {
                last_digit = Some(1);
            } else if sliced.starts_with("two") || sliced.starts_with("2") {
                last_digit = Some(2);
            } else if sliced.starts_with("three") || sliced.starts_with("3") {
                last_digit = Some(3);
            } else if sliced.starts_with("four") || sliced.starts_with("4") {
                last_digit = Some(4);
            } else if sliced.starts_with("five") || sliced.starts_with("5") {
                last_digit = Some(5);
            } else if sliced.starts_with("six") || sliced.starts_with("6") {
                last_digit = Some(6);
            } else if sliced.starts_with("seven") || sliced.starts_with("7") {
                last_digit = Some(7);
            } else if sliced.starts_with("eight") || sliced.starts_with("8") {
                last_digit = Some(8);
            } else if sliced.starts_with("nine") || sliced.starts_with("9") {
                last_digit = Some(9);
            }

            if last_digit.is_some() && first_digit.is_none() {
                first_digit = last_digit;
            }
        }

        count += first_digit.unwrap() * 10 + last_digit.unwrap();
    }

    println!("Part2: {:?}", count)
}