use std::fs;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let mut lines = contents.lines();
    let mut times = lines.next().unwrap().split_whitespace().skip(1).map(|x| x.parse::<f32>().unwrap());
    let mut records = lines.next().unwrap().split_whitespace().skip(1).map(|x| x.parse::<f32>().unwrap());
    
    let mut result = 1;
    while let (Some(time), Some(record)) = (times.next(), records.next()) {
        // distance = (time - t)*t > record
        // time*t - t**2 - record > 0
        // t**2 - time*t + record < 0
        // (time + sqrt(time**2 - 4*record)) / 2 > 0, (time + sqrt(time**2 - 4*record)) / 2 < 0 (assuming a solution)

        let upper_bound = (((time + (time*time - 4.0*record).sqrt()) / 2.0).ceil() as u32) - 1;
        let lower_bound = (((time - (time*time - 4.0*record).sqrt()) / 2.0).floor() as u32) + 1;

        result *= upper_bound - lower_bound + 1;
    }

    println!("Part1 {:?}", result);
}



fn part2(contents: &str) {
    let mut lines = contents.lines();
    let time = lines.next().unwrap().split_whitespace().skip(1).collect::<Vec<&str>>().join("").parse::<f64>().unwrap();
    let record = lines.next().unwrap().split_whitespace().skip(1).collect::<Vec<&str>>().join("").parse::<f64>().unwrap();

    let upper_bound = (((time + (time*time - 4.0*record).sqrt()) / 2.0).ceil() as u64) - 1;
    let lower_bound = (((time - (time*time - 4.0*record).sqrt()) / 2.0).floor() as u64) + 1;

    println!("Part2: {:?}", upper_bound - lower_bound + 1);
}