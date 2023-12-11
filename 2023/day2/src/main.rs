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

    for line in contents.lines() {
        let game_start_idx = line.find(":").unwrap();
        let id = line["Game ".len()..game_start_idx].parse::<u32>().unwrap();
        let games = line[(game_start_idx +1)..].split(";");

        let mut is_possible = true;
        for game in games {
            for qty in game.split(",") {
                let nb_and_color : Vec<&str> = qty.trim().split(" ").collect();
                let nb = nb_and_color[0].parse::<u32>().unwrap();
                let color = nb_and_color[1];

                match color {
                    "red" => {
                        if nb > 12 {
                            is_possible = false;
                        }
                    },
                    "green" => {
                        if nb > 13 {
                            is_possible = false;
                        }
                    },
                    "blue" => {
                        if nb > 14 {
                            is_possible = false;
                        }
                    },
                    _ => {
                        is_possible = false;
                    }
                }
            }
        }

        if is_possible {
            count += id;
        }
    }

    println!("Part1: {}", count);
}

fn part2(contents: &str) {
    let mut power = 0;

    for line in contents.lines() {
        let games = line[(line.find(":").unwrap() +1)..].split(";");

        let mut max_red = 0;
        let mut max_green = 0;
        let mut max_blue = 0;
        for game in games {
            for qty in game.split(",") {
                let nb_and_color : Vec<&str> = qty.trim().split(" ").collect();
                let nb = nb_and_color[0].parse::<u32>().unwrap();
                let color = nb_and_color[1];

                match color {
                    "red" => {
                        if nb > max_red {
                            max_red = nb;
                        }
                    },
                    "green" => {
                        if nb > max_green {
                            max_green = nb;
                        }
                    },
                    "blue" => {
                        if nb > max_blue {
                            max_blue = nb;
                        }
                    },
                    _ => {
                    }
                }
            }
        }

        power += max_blue * max_green * max_red;
    }

    println!("Part2: {}", power);
}