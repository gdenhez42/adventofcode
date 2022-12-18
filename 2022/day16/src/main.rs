use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Not;
use rand::thread_rng;
use rand::seq::SliceRandom;

#[derive(Debug)]
struct Valve {
    tunnels: Vec<String>,
    rate: i32
}

fn calculate_distances(name: &String, valves: &HashMap<String, Valve>) -> HashMap<String, i32> {
    let mut distances: HashMap<String, i32> = HashMap::new();
    let mut next = HashSet::new();
    let mut distance = 0;
    next.insert(name.clone());
    while next.len() > 0 {

        let mut nextnext = HashSet::new();
        for x in &next {
            distances.insert(x.to_string(), distance);
        }
        for x in &next {
            let tunnels = &valves.get(x).unwrap().tunnels;
            for tunnel in tunnels {
                if distances.contains_key(tunnel).not() {
                    nextnext.insert(tunnel.clone());
                }
            }
        }
        next.clear();
        next = nextnext;

        distance += 1;
    }

    distances
}

fn released_pressure(
    valves: &HashMap<String, Valve>,
    distances: &HashMap<String, HashMap<String, i32>>,
    open_order: &Vec<String>,
    max_minute: i32
    ) -> i32
{
    let mut minute = 0;
    let mut valve_num = 0;
    let start_pos = String::from("AA");
    let mut current_pos = &start_pos;
    let mut result = 0;

    while minute < max_minute && valve_num < open_order.len() {
        let nb_minute_spent = distances.get(current_pos)
        .unwrap()
        .get(&open_order[valve_num])
        .unwrap() + 1;
        let nb_minutes_open = max_minute - minute - nb_minute_spent;
        if nb_minutes_open > 0 {
            result += nb_minutes_open*valves.get(&open_order[valve_num]).unwrap().rate;
        }
        minute += nb_minute_spent;
        current_pos = &open_order[valve_num];
        valve_num += 1;
    }

    result
}

fn hill_climbing_pressure(
    valves: &HashMap<String, Valve>,
    distances: &HashMap<String, HashMap<String, i32>>,
    to_open: &Vec<String>,
    max_minute: i32) -> i32
{
    let mut stochastic_max = 0;

    for _tries in 0..10 {
        let mut current_order = to_open.clone();
        current_order.shuffle(&mut thread_rng());

        let mut done = false;
        let mut max = released_pressure(valves, distances, &current_order, max_minute);

        while done.not() {

            let mut i = 0;
            let mut next_order = current_order.clone();
            let mut found = false;
            while i < to_open.len() {
                let mut j = i + 1;
                while j < to_open.len() {
                    let mut neighbor_order = current_order.clone();
                    let temp = neighbor_order[i].clone();
                    neighbor_order[i] = neighbor_order[j].clone();
                    neighbor_order[j] = temp;

                    let candidate_pressure = released_pressure(valves, distances, &neighbor_order, max_minute);
                    if candidate_pressure > max {
                        max = candidate_pressure;
                        next_order = neighbor_order;
                        found = true;
                    }
                    j += 1;

                }
                i += 1;

            }

            if found.not() {
                done = true;
            }

            current_order = next_order;
        }

        if stochastic_max < max {
            stochastic_max = max;
        }
    }
    
    stochastic_max
}

fn hill_climbing_pressure_part1(
    valves: &HashMap<String, Valve>,
    distances: &HashMap<String, HashMap<String, i32>>) -> i32
{
    let mut to_open = Vec::new();
    for (name, valve) in valves {
        if valve.rate > 0 {
            to_open.push(name.clone())
        }
    }

    hill_climbing_pressure(valves, distances, &to_open, 30)
}

fn hill_climbing_pressure_part2(
    valves: &HashMap<String, Valve>,
    distances: &HashMap<String, HashMap<String, i32>>) -> i32
{
    let mut to_open_me = Vec::new();
    let mut to_open_elephant = Vec::new();

    for (name, valve) in valves {
        if valve.rate > 0 {
            if to_open_me.len() < to_open_elephant.len() {
                to_open_me.push(name.clone());
            } else {
                to_open_elephant.push(name.clone())
            }
        }
    }
    let my_pressure = hill_climbing_pressure(valves, distances, &to_open_me, 26);
    let elephant_pressure = hill_climbing_pressure(valves, distances, &to_open_elephant, 26);

    let mut max = my_pressure + elephant_pressure;
    let mut done = false;

    while done.not() {

        // Neighbor: swap one of the elephant with me
        let mut found = false;
        let mut i = 0;
        let mut next_to_open_me = to_open_me.clone();
        let mut next_to_open_elephant = to_open_elephant.clone();
        while i < to_open_me.len() {
            let mut j = 0;
            while j < to_open_elephant.len() {
                let mut neighbor_to_open_me = to_open_me.clone();
                let mut neighbor_to_open_elephant = to_open_elephant.clone();
                let temp = neighbor_to_open_me[i].clone();
                neighbor_to_open_me[i] = neighbor_to_open_elephant[j].clone();
                neighbor_to_open_elephant[j] = temp;
                let neighbor_my_pressure = hill_climbing_pressure(valves, distances, &neighbor_to_open_me, 26);
                let neighbor_elephant_pressure = hill_climbing_pressure(valves, distances, &neighbor_to_open_elephant, 26);
                if neighbor_my_pressure + neighbor_elephant_pressure > max {
                    max = neighbor_my_pressure + neighbor_elephant_pressure;
                    found = true;
                    next_to_open_me = neighbor_to_open_me;
                    next_to_open_elephant = neighbor_to_open_elephant;
                }
                j += 1;
            }
            i += 1;
        }

        // Neighbor: elephant give one to me
        i = 0;
        while i < to_open_elephant.len() {
            let mut neighbor_to_open_me = to_open_me.clone();
            let mut neighbor_to_open_elephant = to_open_elephant.clone();
            neighbor_to_open_me.push(to_open_elephant[i].clone());
            neighbor_to_open_elephant.remove(i);
            let neighbor_my_pressure = hill_climbing_pressure(valves, distances, &neighbor_to_open_me, 26);
            let neighbor_elephant_pressure = hill_climbing_pressure(valves, distances, &neighbor_to_open_elephant, 26);
            if neighbor_my_pressure + neighbor_elephant_pressure > max {
                max = neighbor_my_pressure + neighbor_elephant_pressure;
                found = true;
                next_to_open_me = neighbor_to_open_me;
                next_to_open_elephant = neighbor_to_open_elephant;
            }
            i += 1;
        }

        // Neighbor: me give one to elephant
        i = 0;
        while i < to_open_me.len() {
            let mut neighbor_to_open_me = to_open_me.clone();
            let mut neighbor_to_open_elephant = to_open_elephant.clone();
            neighbor_to_open_elephant.push(to_open_me[i].clone());
            neighbor_to_open_me.remove(i);
            let neighbor_my_pressure = hill_climbing_pressure(valves, distances, &neighbor_to_open_me, 26);
            let neighbor_elephant_pressure = hill_climbing_pressure(valves, distances, &neighbor_to_open_elephant, 26);
            if neighbor_my_pressure + neighbor_elephant_pressure > max {
                max = neighbor_my_pressure + neighbor_elephant_pressure;
                found = true;
                next_to_open_me = neighbor_to_open_me;
                next_to_open_elephant = neighbor_to_open_elephant;
            }
            i += 1;
        }


        to_open_me = next_to_open_me;
        to_open_elephant = next_to_open_elephant;

        if found.not() {
            done = true;
        }

    }

    max
}


fn main() {
    // --snip--
    let contents = fs::read_to_string("input.txt")
        .expect("Should have been able to read the file");

    let lines = contents.lines();

    let mut valves: HashMap<String, Valve> = HashMap::new();
    for line in lines {
        let tokens: Vec<&str> = line.split(" ").collect();
        let rate = tokens[4].strip_prefix("rate=")
                            .and_then(|s| s.strip_suffix(";"))
                            .and_then(|s| Some(s.parse::<i32>().unwrap()))
                            .expect("rate is on format rate={nb};");

        let mut tunnels: Vec<String> = vec![];
        for n in 9..tokens.len() {
            match tokens[n].strip_suffix(',') {
                Some(s) => tunnels.push(String::from(s)),
                None => tunnels.push(String::from(tokens[n]))
            }  
        }
        
        valves.insert(
            String::from(tokens[1]),
            Valve {
                rate: rate,
                tunnels: tunnels
            }
        );
    }

    let mut distances: HashMap<String, HashMap<String, i32>> = HashMap::new();
    let mut max_distance = 0;
    for (name, _valve) in &valves {
        let d = calculate_distances(&name, &valves);
        for (_name, dist) in &d {
            if *dist > max_distance {
                max_distance = *dist;
            }
        } 
        distances.insert(name.to_string(), d);
    }

    println!("{}", hill_climbing_pressure_part1(&valves, &distances));
    println!("{}", hill_climbing_pressure_part2(&valves, &distances));
}

