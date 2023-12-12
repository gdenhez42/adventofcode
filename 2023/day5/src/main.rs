use std::fs;
use std::cmp;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

#[derive(Debug, Copy, Clone)]
struct AlmanacMapItem {
    dest: u64,
    src: u64,
    len: u64
}

fn parse_seeds(sections : &mut std::str::Split<'_, &str>) -> Vec<u64> {
    sections.next()
        .unwrap()
        .split_whitespace()
        .skip(1)
        .map(|s| s.parse::<u64>().unwrap())
        .collect()
}

fn parse_almanac_map(sections : &mut std::str::Split<'_, &str>) -> Vec<AlmanacMapItem> {
    sections.next()
        .unwrap()
        .lines()
        .skip(1)
        .map(|l| -> AlmanacMapItem {
            let mut tokens = l.split_whitespace();
            AlmanacMapItem {
                dest : tokens.next().unwrap().parse::<u64>().unwrap(),
                src : tokens.next().unwrap().parse::<u64>().unwrap(),
                len : tokens.next().unwrap().parse::<u64>().unwrap()
            }
        })
        .collect()
}

fn find_dest(src : u64, map : &Vec<AlmanacMapItem>) -> u64 {
    map.iter().fold(src, |acc, i| -> u64 {
        if src >= i.src && src < i.src + i.len {
            i.dest + (src - i.src)
        } else {
            acc
        }
    })
}

#[derive(Debug)]
struct Range(u64, u64);

fn map_ranges(sources : &Vec<Range>, map : &Vec<AlmanacMapItem>) -> Vec<Range> {
    let mut dests = Vec::new();
    for src in sources {
        let mut cursor = src.0;

        while cursor < src.0 + src.1 {
            let mut almanac_map_item_low : Option<AlmanacMapItem> = None;
            let mut almanac_map_item_hi: Option<AlmanacMapItem> = None;
            for item in map {
                if cursor >= item.src {
                    match almanac_map_item_low {
                        Some(x) if x.src < item.src  => {
                            almanac_map_item_low = Some(*item);
                        },
                        None => {
                            almanac_map_item_low = Some(*item);
                        },
                        _ => {}
                    }
                }

                if cursor < item.src {
                    match almanac_map_item_hi {
                        Some(x) if x.src > item.src  => {
                            almanac_map_item_hi = Some(*item);
                        },
                        None => {
                            almanac_map_item_hi = Some(*item);
                        },
                        _ => {}
                    }
                }
            }
            match (almanac_map_item_low, almanac_map_item_hi) {
                (Some(x), _) if cursor < x.src + x.len => {
                    let end = cmp::min(x.src + x.len, src.0 + src.1);
                    dests.push(Range(x.dest + (cursor - x.src), end - cursor));
                    cursor = end;
                },
                (_, Some(x)) => {
                    let end = cmp::min(x.src, src.0 + src.1);
                    dests.push(Range(cursor, end - cursor));
                    cursor = end;
                }
                (_, None) => {
                    let end = src.0 + src.1;
                    dests.push(Range(cursor, end - cursor));
                    cursor = end;
                }
            }
        }
    }

    dests
}


fn part1(contents: &str) {
    let mut sections = contents.split("\n\n");

    let seeds : Vec<u64> = parse_seeds(&mut sections);
    let seed_to_soil : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let soil_to_fertilizer : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let fertilizer_to_water : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let water_to_light : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let light_to_temperature : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let temperature_to_humidity : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let humidity_to_location : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);

    let min_location = seeds.iter().map(|seed| -> u64 {
        let soil = find_dest(*seed, &seed_to_soil);
        let fertilizer = find_dest(soil, &soil_to_fertilizer);
        let water = find_dest(fertilizer, &fertilizer_to_water);
        let light = find_dest(water, &water_to_light);
        let temperature = find_dest(light, &light_to_temperature);
        let humidity = find_dest(temperature, &temperature_to_humidity);
        find_dest(humidity, &humidity_to_location)
    }).min().unwrap();

    println!("Part1 {:?}", min_location);
}



fn part2(contents: &str) {
    let mut sections = contents.split("\n\n");

    let seeds : Vec<u64> = parse_seeds(&mut sections);
    let seed_to_soil : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let soil_to_fertilizer : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let fertilizer_to_water : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let water_to_light : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let light_to_temperature : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let temperature_to_humidity : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);
    let humidity_to_location : Vec<AlmanacMapItem> = parse_almanac_map(&mut sections);

    let seed_ranges : Vec<Range> = (0..seeds.len()/2).map(|i| Range(seeds[i*2], seeds[i*2 + 1])).collect();
    let soil_ranges = map_ranges(&seed_ranges, &seed_to_soil);
    let fertilizer_ranges = map_ranges(&soil_ranges, &soil_to_fertilizer);
    let water_ranges = map_ranges(&fertilizer_ranges, &fertilizer_to_water);
    let light_ranges = map_ranges(&water_ranges, &water_to_light);
    let temperature_ranges = map_ranges(&light_ranges, &light_to_temperature);
    let humidity_ranges = map_ranges(&temperature_ranges, &temperature_to_humidity);
    let location_ranges = map_ranges(&humidity_ranges, &humidity_to_location);

    let min_location = location_ranges.iter().map(|r| r.0).min().unwrap();

    println!("Part2: {:?}", min_location);
}