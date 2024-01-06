use std::fs;
use std::collections::HashSet;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {
    let map = contents.lines().map(|l| l.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    let mut start = (0,0);
    for i in 0..map[0].len() {
        if map[0][i] == '.' {
            start.1 = i;
        }
    }

    let mut end = (map.len()-1,0);
    for i in 0..map[map.len()-1].len() {
        if map[map.len()-1][i] == '.' {
            end.1 = i;
        }
    }

    println!("Part1: {:?}", longest_hike_with_slopes(&map, start, end, HashSet::new(), 0));
}

fn part2(contents: &str) {
    let map = contents.lines().map(|l| l.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    let mut start = (0,0);
    for i in 0..map[0].len() {
        if map[0][i] == '.' {
            start.1 = i;
        }
    }

    let intersections = get_intersections(&map);

    let graph = map_to_graph(&map, &intersections, None, start, &mut HashSet::new());

    println!("Part2: {:?}", longest_hike_no_slopes(&graph, &graph[0], None, HashSet::new(), 0));
}

fn longest_hike_with_slopes(map : &Vec<Vec<char>>, mut current_pos : (usize, usize), goal : (usize, usize), mut visited : HashSet<(usize, usize)>, mut result : u64) -> Option<u64> {
    loop {
        if current_pos == goal {
            break Some(result)
        }

        visited.insert(current_pos);
        let mut possible_next_pos = Vec::new();
        if current_pos.0 > 0 &&
           map[current_pos.0-1][current_pos.1] != '#' &&
           map[current_pos.0-1][current_pos.1] != 'v' &&
           !visited.contains(&(current_pos.0-1, current_pos.1))
        {
            possible_next_pos.push((current_pos.0-1, current_pos.1));
        }

        if current_pos.0 < map.len() - 1 &&
           map[current_pos.0+1][current_pos.1] != '#' &&
           map[current_pos.0+1][current_pos.1] != '^' &&
           !visited.contains(&(current_pos.0+1, current_pos.1))
        {
            possible_next_pos.push((current_pos.0+1, current_pos.1));
        }

        if current_pos.1 > 0 &&
           map[current_pos.0][current_pos.1-1] != '#' &&
           map[current_pos.0][current_pos.1-1] != '>' &&
           !visited.contains(&(current_pos.0, current_pos.1-1))
        {
            possible_next_pos.push((current_pos.0, current_pos.1-1));
        }

        if current_pos.1 < map[current_pos.0].len() - 1 &&
           map[current_pos.0][current_pos.1+1] != '#' &&
           map[current_pos.0][current_pos.1+1] != '<' &&
           !visited.contains(&(current_pos.0, current_pos.1+1))
        {
            possible_next_pos.push((current_pos.0, current_pos.1+1));
        }

        if possible_next_pos.len() == 0 {
            println!("Current pos: {:?} - No next pos, goal: {:?}", current_pos, goal);
            break None
        }

        if possible_next_pos.len() > 1 {
            break possible_next_pos.iter().filter_map(|pos| longest_hike_with_slopes(map, pos.clone(), goal, visited.clone(), result + 1)).max();
        }

        current_pos = possible_next_pos[0];
        result += 1;
    }   
}

fn longest_hike_no_slopes(
    graph : &Vec<Segment>,
    current: &Segment,
    intersection: Option<(usize, usize)>,
    mut visited : HashSet<(usize,usize)>,
    result: usize) -> Option<usize> {

    if intersection.is_some() {
        visited.insert(intersection.unwrap());
    }

    if current.i2 == None {
        Some(result + current.l)
    } else {
        let next_intersection = if current.i1 == intersection { current.i2 } else { current.i1 };

        let max = graph.iter()
            .filter(|&s| s != current && ((s.i1 == next_intersection && s.i2.map_or(true, |i| !visited.contains(&i))) || (s.i2 == next_intersection && s.i1.map_or(true, |i| !visited.contains(&i)))))
            .filter_map(|s| longest_hike_no_slopes(graph, s, next_intersection, visited.clone(), result + current.l + 1))
            .max();

        max
    }
}


#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct Segment {
    i1 : Option<(usize, usize)>,
    i2 : Option<(usize, usize)>,
    l : usize
}

fn get_intersections(map : &Vec<Vec<char>>) -> HashSet<(usize,usize)> {
    let mut intersections = HashSet::new();
    for i in 0..map.len() {
        for j in 0..map[i].len() {
            if map[i][j] != '#' {
                let mut temp = Vec::new();
            
                if i > 0 && map[i-1][j] != '#'
                {
                    temp.push((i-1, j));
                }

                if i < map.len() - 1 && map[i+1][j] != '#'
                {
                    temp.push((i+1, j));
                }

                if j > 0 && map[i][j-1] != '#'
                {
                    temp.push((i, j-1));
                }

                if j < map[i].len() - 1 && map[i][j+1] != '#'
                {
                    temp.push((i, j+1));
                }

                if temp.len() > 2 {
                    intersections.insert((i,j));
                }
            }
        }
    }
    intersections
}

fn map_to_graph(
    map : &Vec<Vec<char>>,
    intersections : &HashSet<(usize, usize)>,
    i1 : Option<(usize, usize)>,
    mut pos : (usize, usize),
    visited : &mut HashSet<(usize, usize)>) -> Vec<Segment>
{
    let mut result = Vec::new();
    
    let mut l = 0;
    loop {

        let mut temp = Vec::new();
        
        if pos.0 > 0 &&
           map[pos.0-1][pos.1] != '#' &&
           i1 != Some((pos.0-1, pos.1)) &&
           !visited.contains(&(pos.0-1, pos.1))
        {
            temp.push((pos.0-1, pos.1));
        }

        if pos.0 < map.len() - 1 &&
           map[pos.0+1][pos.1] != '#' &&
           i1 != Some((pos.0+1, pos.1)) &&
           !visited.contains(&(pos.0+1, pos.1))
        {
            temp.push((pos.0+1, pos.1));
        }

        if pos.1 > 0 &&
           map[pos.0][pos.1-1] != '#' &&
           i1 != Some((pos.0, pos.1-1)) &&
           !visited.contains(&(pos.0, pos.1-1))
        {
            temp.push((pos.0, pos.1-1));
        }

        if pos.1 < map[pos.0].len() - 1 &&
           map[pos.0][pos.1+1] != '#' &&
           i1 != Some((pos.0, pos.1+1)) &&
           !visited.contains(&(pos.0, pos.1+1))
        {
            temp.push((pos.0, pos.1+1));
        }

        if intersections.contains(&pos) {
            result.push(Segment {
                i1 : i1,
                i2 : Some(pos),
                l : l
            });
            for p in temp {
                if !visited.contains(&p) {
                    result.extend(map_to_graph(map, intersections, Some(pos), p, visited));
                }
            }
            break result
        }

        if temp.len() == 0 {
            if pos.0 == map.len() - 1 {
                result.push(Segment {
                    i1 : i1,
                    i2 : None,
                    l : l
                });
            }
            break result
        }

        visited.insert(pos);
        l += 1;

        pos = temp[0];
    }   

}
