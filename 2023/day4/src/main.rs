use std::fs;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let mut sum = 0;
    for line in contents.lines() {
        let [_, winning_numbers_section, numbers_section] = line.split(['|', ':']).collect::<Vec<_>>()[..] else { panic!("Unexpected input") };
        let winning_numbers = winning_numbers_section.split_whitespace().map(|s| s.parse::<u64>().unwrap()).collect::<Vec<_>>();

        sum += numbers_section.split_whitespace().map(|s| s.parse::<u64>().unwrap())
            .fold(0, |acc, x| -> u32 {
                if winning_numbers.contains(&x) {
                    if acc == 0 {
                        1
                    } else {
                        acc * 2
                    }
                } else {
                    acc
                }
            });
    }
    println!("Part1 {:?}", sum);
}

fn part2(contents: &str) {

    let mut counts : Vec<u32> = contents.lines().map(|_| 1).collect();

    for (i, line) in contents.lines().enumerate() {
        let [_, winning_numbers_section, numbers_section] = line.split(['|', ':']).collect::<Vec<_>>()[..] else { panic!("Unexpected input") };
        let winning_numbers = winning_numbers_section.split_whitespace().map(|s| s.parse::<u64>().unwrap()).collect::<Vec<_>>();

        let nb_winning = numbers_section.split_whitespace().map(|s| s.parse::<u64>().unwrap())
            .fold(0, |acc, x| -> usize {
                if winning_numbers.contains(&x) {
                    acc + 1
                } else {
                    acc
                }
            });
        for j in (i+1)..(i+1+nb_winning) {
            counts[j] += counts[i]
        }
        
    }
    println!("Part2 {:?}", counts.iter().fold(0, |acc, x| acc + x));
}