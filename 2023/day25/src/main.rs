use std::fs;
use std::collections::HashSet;
use std::collections::HashMap;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let mut machines = HashSet::new();
    let mut wires : HashMap<String, HashSet<String>> = HashMap::new();
    let mut to_disconnect = Vec::new();
    for line in contents.lines() {
        let [o, ds] = line.split(":").collect::<Vec<&str>>()[..] else { panic!("unexpected input!") };
        machines.insert(String::from(o));
        for d in ds.split(' ').skip(1) {
            machines.insert(String::from(d));
            to_disconnect.push((String::from(o), String::from(d)));
            wires.entry(String::from(o)).and_modify(|s| { s.insert(String::from(d)); }).or_insert(HashSet::from([String::from(d)]));
            wires.entry(String::from(d)).and_modify(|s| { s.insert(String::from(o)); }).or_insert(HashSet::from([String::from(o)]));
        }
    }

    let mut tried = HashSet::new();
    for i in 0..to_disconnect.len() {
        tried.insert(to_disconnect[i].clone());
        let GroupKind::One(v1) = group_kind(&machines, &wires, HashSet::from([to_disconnect[i].clone()])) else { panic!("Distinct groups when removing only one node"); };

        for d1 in v1.iter().filter(|x| !tried.contains(x)) {
            let GroupKind::One(v2) = group_kind(&machines, &wires, HashSet::from([to_disconnect[i].clone(), d1.clone()])) else { panic!("Distinct groups when removing only two nodes"); };

            for d2 in v2.iter().filter(|x| !tried.contains(x)) {
                let disconnected = HashSet::from([to_disconnect[i].clone(), d1.clone(), d2.clone()]);
                match group_kind(&machines, &wires, disconnected) {
                    GroupKind::Distinct(s1,s2) => {
                        println!("Part1: {:?}", s1*s2);
                    },
                    _ => {}
                }
            }
        }
    }
}

fn part2(_contents: &str) {
    println!("part2: {:?}", 42);
}

enum GroupKind {
    Distinct(usize, usize),
    One(Vec<(String, String)>)
}

fn group_kind(machines: &HashSet<String>, wires: &HashMap<String, HashSet<String>>, disconnected: HashSet<(String,String)>) -> GroupKind {
    let first_disconnect = disconnected.iter().next().unwrap();
    let start = first_disconnect.0.clone();
    let goal = first_disconnect.1.clone();

    let mut visited = HashMap::new();
    visited.insert(start.clone(), None);
    let mut current = HashSet::from([start]);
    loop {
        let mut temp = HashSet::new();
        for c in current {
            for d in wires.get(&c).unwrap() {
                if !visited.contains_key(d) && !disconnected.contains(&(c.clone(),d.clone())) && !disconnected.contains(&(d.clone(),c.clone())) {
                    temp.insert(d.clone());
                    visited.insert(d.clone(), Some(c.clone()));
                }
            }
        }

        if visited.contains_key(&goal) {
            let mut path = &goal;
            let mut potential_disconnect = Vec::new();
            while let Some(Some(i)) = visited.get(path) {
                potential_disconnect.push((i.clone(), path.clone()));
                path = i;
            }
            break GroupKind::One(potential_disconnect);
        }

        if temp.len() == 0 {
            break GroupKind::Distinct(visited.len(), machines.len()-visited.len());
        }

        current = temp;
    }
}

