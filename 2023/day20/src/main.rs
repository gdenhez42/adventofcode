use std::fs;
use std::collections::HashMap;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    // Parse modules
    let mut modules = parse_modules(contents);

    let mut nb_hi = 0;
    let mut nb_lo = 0;
    for _ in 0..1000 {
        nb_lo += 1;
        let mut pulses : Vec<Pulse> = vec![Pulse(0, "button".to_string(), "broadcaster".to_string())];
        while pulses.len() > 0 {
            let mut temp : Vec<Pulse> = Vec::new();
            for pulse in pulses {
                match modules.get_mut(&pulse.2) {
                    Some(&mut Module::Broadcaster(ref outputs)) => {
                        for o in outputs {
                            temp.push(Pulse(pulse.0, pulse.2.to_string(), o.to_string()));
                        }
                        if pulse.0 == 0 {
                            nb_lo += outputs.len();
                        } else {
                            nb_hi += outputs.len();
                        }
                    },
                    Some(&mut Module::FlipFlop(ref mut state, ref outputs)) => {
                        if pulse.0 == 0 {
                            if state.0 {
                                for o in outputs {
                                    temp.push(Pulse(0, pulse.2.to_string(), o.to_string()));
                                }
                                nb_lo += outputs.len();
                            } else {
                                for o in outputs {
                                    temp.push(Pulse(1, pulse.2.to_string(), o.to_string()));
                                }
                                nb_hi += outputs.len();
                            }
                            state.0 = !state.0;
                        }
                    },
                    Some(&mut Module::Conjunction(ref mut state, ref outputs)) => {
                        state.insert(pulse.1.to_string(), pulse.0);
                        if state.iter().all(|(_, &v)| v == 1 ) {
                            for o in outputs {
                                temp.push(Pulse(0, pulse.2.to_string(), o.to_string()));
                            }
                            nb_lo += outputs.len();
                        } else {
                            for o in outputs {
                                temp.push(Pulse(1, pulse.2.to_string(), o.to_string()));
                            }
                            nb_hi += outputs.len();
                        }
                    },
                    _ => {}
                }
            }
            pulses = temp;
        }
    }

    println!("Part1: {:?}", nb_hi*nb_lo);
}

fn part2(contents: &str) {
    // Parse modules
    let mut modules = parse_modules(contents);

    // (Quite) a bit of hardcoding... really don't know how to solve this one otherwise :/
    let hn_freq = high_pulse_frequency("hn", modules.clone());
    let tg_freq = high_pulse_frequency("tg", modules.clone());
    let lz_freq = high_pulse_frequency("lz", modules.clone());
    let kh_freq = high_pulse_frequency("kh", modules.clone());

    println!("Part2: {:?}", smallest_common_multiple(&vec![hn_freq, tg_freq, lz_freq, kh_freq]));
}

#[derive(Debug, Clone)]
enum Module {
    FlipFlop(FlipFlopState, Vec<String>),
    Conjunction(HashMap<String, u8>, Vec<String>),
    Broadcaster(Vec<String>)
}

#[derive(Debug, Clone)]
struct Pulse(u8, String, String);

#[derive(Debug, Clone)]
struct FlipFlopState(bool);


fn parse_modules(contents: &str) -> HashMap<String, Module> {
    let mut modules : HashMap<String, Module> = HashMap::new();
    let mut inputs : HashMap<String, Vec<String>> = HashMap::new();
    for line in contents.lines() {
        let [def, outputs_str] = line.split(" -> ").collect::<Vec<&str>>()[..] else { panic!("unexpected input!");};
        let outputs = outputs_str.split(", ").map(|o| o.to_string()).collect::<Vec<String>>();
        let input = if def == "broadcaster" { def } else { &def[1..] };

        for o in &outputs {
            inputs.entry(o.to_string()).and_modify(|i| i.push(input.to_string())).or_insert(vec![input.to_string()]);
        }

        if def.starts_with("%") {
            modules.insert(input.to_string(), Module::FlipFlop(FlipFlopState(false), outputs));
        } else if def.starts_with("&") {
            modules.insert(input.to_string(), Module::Conjunction(HashMap::new(), outputs));
        } else {
            modules.insert(input.to_string(), Module::Broadcaster(outputs));
        }
    }

    for (o, i) in inputs {
        modules.entry(o)
            .and_modify(|module| -> () {
                match module {
                    Module::Conjunction(map, _) => {
                        for input in i {
                            map.insert(input, 0);
                        }
                    },
                    _ => {}
                }
            });
    }

    modules
}

fn high_pulse_frequency(module: &str, mut modules: HashMap<String, Module>) -> u64 {
    let mut emitted = false;
    let mut nb_button_press = 0;
    while !emitted {
        let mut pulses : Vec<Pulse> = vec![Pulse(0, "button".to_string(), "broadcaster".to_string())];
        nb_button_press += 1;
        while pulses.len() > 0 {
            let mut temp : Vec<Pulse> = Vec::new();
            for pulse in pulses {
                if pulse.1 == module && pulse.0 == 1 {
                    emitted = true;
                } 

                match modules.get_mut(&pulse.2) {
                    Some(&mut Module::Broadcaster(ref outputs)) => {
                        for o in outputs {
                            temp.push(Pulse(pulse.0, pulse.2.to_string(), o.to_string()));
                        }
                    },
                    Some(&mut Module::FlipFlop(ref mut state, ref outputs)) => {
                        if pulse.0 == 0 {
                            if state.0 {
                                for o in outputs {
                                    temp.push(Pulse(0, pulse.2.to_string(), o.to_string()));
                                }
                            } else {
                                for o in outputs {
                                    temp.push(Pulse(1, pulse.2.to_string(), o.to_string()));
                                };
                            }
                            state.0 = !state.0;
                        }
                    },
                    Some(&mut Module::Conjunction(ref mut state, ref outputs)) => {
                        state.insert(pulse.1.to_string(), pulse.0);
                        if state.iter().all(|(_, &v)| v == 1 ) {
                            for o in outputs {
                                temp.push(Pulse(0, pulse.2.to_string(), o.to_string()));
                            }
                        } else {
                            for o in outputs {
                                temp.push(Pulse(1, pulse.2.to_string(), o.to_string()));
                            }
                        }
                    },
                    _ => {}
                }
            }

            pulses = temp;
        }
    }
    nb_button_press
}

fn prime_decomposition(mut nb : u64) -> Vec<u64> {
    let mut primes = Vec::new();
    let sqrt = ((nb as f64).sqrt() as u64) + 1;
    for i in 2..sqrt {
        while nb % i == 0 {
            primes.push(i);
            nb = nb / i;
        }

    }
    if nb > 1 {
        primes.push(nb)
    }
    primes
}

fn merge_primes(p1 : &Vec<u64>, p2 : &Vec<u64>) -> Vec<u64> {
    let mut result = Vec::new();

    let mut i1 = 0;
    let mut i2 = 0;
    while i1 < p1.len() && i2 < p2.len() {
        if p1[i1] == p2[i2] {
            result.push(p1[i1]);
            i1 += 1;
            i2 += 1;
        } else if p1[i1] < p2[i2] {
            result.push(p1[i1]);
            i1 += 1;
        } else {
            result.push(p2[i2]);
            i2 += 1;
        }
    }
    while i1 < p1.len() {
        result.push(p1[i1]);
        i1 += 1;
    }
    while i2 < p2.len() {
        result.push(p2[i2]);
        i2 += 1;
    }

    result
}

fn smallest_common_multiple(nbs : &Vec<u64>) -> u64 {
    let mut primes = Vec::new();
    for nb in nbs {
        let p = prime_decomposition(*nb);
        primes = merge_primes(&p, &primes);
    }
    primes.iter().fold(1, |x, acc| x * acc)
}