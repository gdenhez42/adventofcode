use std::fs;
use std::cmp::Ordering;

#[derive(Debug)]
struct Hand {
    cards: String,
    bid: u64,
    kind: u32
}

fn get_kind(cards: &str) -> u32 {
    let mut sorted = cards.chars().collect::<Vec<char>>();
    sorted.sort();
    let mut pairs : Vec<(char, u32)> = Vec::new();
    for s in sorted {
        match pairs.last() {
            Some((x, nb)) if *x == s => {
                let l = pairs.len();
                pairs[l - 1] = (s, nb + 1);
            },
            _ => {
                pairs.push((s, 1));
            }
        }
    }
    pairs.sort_by(|a, b| b.1.cmp(&a.1));
    match pairs.as_slice() {
        [(_, 5)] => 7,
        [(_, 4), (_, 1)] => 6,
        [(_, 3), (_, 2)] => 5,
        [(_, 3), _rest @ ..] => 4,
        [(_, 2), (_, 2), (_, 1)] => 2,
        [(_, 2), _rest @ ..] => 1,
        _ => 0
    }
}

fn get_kind_with_jokers(cards: &str) -> u32 {
    let mut sorted = cards.chars().collect::<Vec<char>>();
    sorted.sort();
    let mut pairs : Vec<(char, u32)> = Vec::new();
    let mut jokers : u32 = 0;
    for s in sorted {
        match (s, pairs.last()) {
            ('J', _) => {
                jokers += 1;
            }
            (_, Some((x, nb))) if *x == s => {
                let l = pairs.len();
                pairs[l - 1] = (s, nb + 1);
            },
            _ => {
                pairs.push((s, 1));
            }
        }
    }
    if pairs.len() == 0 {
        7
    } else {
        pairs.sort_by(|a, b| b.1.cmp(&a.1));
        let first_pair = pairs[0];
        pairs[0] = (first_pair.0, first_pair.1 + jokers);
        match pairs.as_slice() {
            [(_, 5)] => 7,
            [(_, 4), (_, 1)] => 6,
            [(_, 3), (_, 2)] => 5,
            [(_, 3), _rest @ ..] => 4,
            [(_, 2), (_, 2), (_, 1)] => 2,
            [(_, 2), _rest @ ..] => 1,
            _ => 0
        }
    }
}

fn get_numeric_value(c : char) -> u32 {
    match c {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' => 11,
        'T' => 10,
        _ => (c as u32) - ('0' as u32)
    }
}

fn get_numeric_value_with_jokers(c : char) -> u32 {
    match c {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' => 1,
        'T' => 10,
        _ => (c as u32) - ('0' as u32)
    }
}

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}


fn part1(contents: &str) {
    let mut hands: Vec<Hand> = contents.lines().map(|line| -> Hand {
        let [cards, bid] = line.split_whitespace().collect::<Vec<&str>>()[..] else { panic!("Unexpected input!") };
        Hand { 
            cards: String::from(cards),
            bid: bid.parse::<u64>().unwrap(),
            kind: get_kind(cards)
        }
    }).collect();

    hands.sort_by(|a, b| -> Ordering {
        if a.kind < b.kind {
            Ordering::Less
        } else if a.kind > b.kind {
            Ordering::Greater
        } else {
            a.cards.chars().zip(b.cards.chars()).fold(Ordering::Equal, |acc, (c1, c2)| -> Ordering {
                let nb1 = get_numeric_value(c1);
                let nb2 = get_numeric_value(c2);
                if acc == Ordering::Equal && nb1 < nb2 {
                    Ordering::Less
                } else if acc == Ordering::Equal && nb1 > nb2 {
                    Ordering::Greater
                } else {
                    acc
                }
            })
        }
    });

    let result = hands.iter().enumerate().fold(0, |acc, (i, x)| acc + ((i as u64 + 1)*x.bid));

    println!("Part1 {:?}", result);
}



fn part2(contents: &str) {
    let mut hands: Vec<Hand> = contents.lines().map(|line| -> Hand {
        let [cards, bid] = line.split_whitespace().collect::<Vec<&str>>()[..] else { panic!("Unexpected input!") };
        Hand { 
            cards: String::from(cards),
            bid: bid.parse::<u64>().unwrap(),
            kind: get_kind_with_jokers(cards)
        }
    }).collect();

    hands.sort_by(|a, b| -> Ordering {
        if a.kind < b.kind {
            Ordering::Less
        } else if a.kind > b.kind {
            Ordering::Greater
        } else {
            a.cards.chars().zip(b.cards.chars()).fold(Ordering::Equal, |acc, (c1, c2)| -> Ordering {
                let nb1 = get_numeric_value_with_jokers(c1);
                let nb2 = get_numeric_value_with_jokers(c2);
                if acc == Ordering::Equal && nb1 < nb2 {
                    Ordering::Less
                } else if acc == Ordering::Equal && nb1 > nb2 {
                    Ordering::Greater
                } else {
                    acc
                }
            })
        }
    });

    let result = hands.iter().enumerate().fold(0, |acc, (i, x)| acc + ((i as u64 + 1)*x.bid));
    println!("Part2: {:?}", result);
}