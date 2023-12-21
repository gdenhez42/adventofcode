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
    for step in contents.split(',') {
        result += hash(step);
    }
    println!("Part1: {:?}", result);
}

fn part2(contents: &str) {
    let mut hashmap : Vec<Vec<Lens>> = vec![Vec::new(); 256];
    for step in contents.split(',') {
        match step.find('=') {
            Some(i) => {
                let label = &step[0..i];
                let hash = hash(label) as usize;
                let focal = &step[i+1..].parse::<usize>().unwrap();

                let mut replaced = false;
                for j in 0..hashmap[hash].len() {
                    if hashmap[hash][j].label == label {
                        hashmap[hash][j] = Lens {label : String::from(label), focal: *focal};
                        replaced = true;
                    }
                }

                if !replaced {
                    hashmap[hash].push(Lens {label : String::from(label), focal: *focal});
                }
            },
            None => {
                let label = &step[0..step.len()-1];
                let hash = hash(label) as usize;
                let mut position = None;
                for j in 0..hashmap[hash].len() {
                    if hashmap[hash][j].label == label {
                        position = Some(j)
                    }
                }

                if let Some(j) = position {
                    hashmap[hash].remove(j);
                }
            }
        }
    }

    let mut result = 0;
    for i in 0..256 {
        for (j, elem) in hashmap[i].iter().enumerate() {
            result += (i+1) * (j+1) * elem.focal;
        }
    }

    println!("Part2: {:?}", result);
}

fn hash(s : &str) -> u32 {
    let mut result = 0;
    for c in s.chars() {
        result += c as u32;
        result *= 17;
        result %= 256;
    }
    result
}

#[derive(Debug, Clone)]
struct Lens {
    label: String,
    focal: usize
}