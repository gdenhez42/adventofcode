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
    let map = contents.lines().map(|c| c.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();

    let mut starting_pos = None;
    for i in 0..map.len() {
        for j in 0..map[i].len() {
            if map[i][j] == 'S' {
                starting_pos = Some((i, j));
            }
        }
    }

    let mut new_dests = Vec::new();
    new_dests.push(starting_pos.unwrap());
    for _ in 0..64 {
        let mut possible_dests = HashSet::new();
        let mut temp = Vec::new();
        for d in new_dests {
            if d.0 > 0 && map[d.0-1][d.1] != '#' && possible_dests.insert((d.0-1, d.1)) {
                temp.push((d.0-1, d.1));
            }
            if d.0 < map.len() - 1 && map[d.0+1][d.1] != '#' && possible_dests.insert((d.0+1, d.1)) {
                temp.push((d.0+1, d.1));
            }
            if d.1 > 0 && map[d.0][d.1-1] != '#' && possible_dests.insert((d.0, d.1-1)) {
                temp.push((d.0, d.1-1));
            }
            if d.1 < map[0].len() - 1 && map[d.0][d.1+1] != '#' && possible_dests.insert((d.0, d.1+1)) {
                temp.push((d.0, d.1+1));
            }
        }
        new_dests = temp;
    }

    println!("Part1: {:?}", new_dests.len());
}

fn part2(contents: &str) {

    let map = contents.lines().map(|c| c.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();
    let l = map.len();

    let starting_pos = (l/2, l/2);
    let vertical_corridors = vertical_corridors(&map);
    let horizontal_corridors = horizontal_corridors(&map);
    let distances = distances_from_corridors(&map, &vertical_corridors, &horizontal_corridors);

    let nb_steps = 26501365;
    let mut result = 0;

    for i in 0..l {
        for j in 0..l {
            if map[i][j] != '#' && distances.get(&(0,0)).unwrap().get(&(i,j)).is_some() {
                let is_even = (i + j) % 2 == 0;
                if (is_even && nb_steps % 2 == 0) || (!is_even && nb_steps % 2 == 1) {
                    result += 1;
                }

                // North
                let min_dist_north = vertical_corridors.iter()
                    .map(|v1| distances.get(&(0,*v1)).unwrap().get(&starting_pos).unwrap() + distances.get(&(l-1,*v1)).unwrap().get(&(i,j)).unwrap() + 1)
                    .min()
                    .unwrap();
                result += nb_strait(nb_steps, min_dist_north, l, is_even);

                // South
                let min_dist_south = vertical_corridors.iter()
                    .map(|v1| distances.get(&(l-1,*v1)).unwrap().get(&starting_pos).unwrap() + distances.get(&(0,*v1)).unwrap().get(&(i,j)).unwrap() + 1)
                    .min()
                    .unwrap();
                result += nb_strait(nb_steps, min_dist_south, l, is_even);

                // East
                let min_dist_east = horizontal_corridors.iter()
                    .map(|v1| distances.get(&(*v1,l-1)).unwrap().get(&starting_pos).unwrap() + distances.get(&(*v1,0)).unwrap().get(&(i,j)).unwrap() + 1)
                    .min()
                    .unwrap();
                result += nb_strait(nb_steps, min_dist_east, l, is_even);

                // West
                let min_dist_west = horizontal_corridors.iter()
                    .map(|v1| distances.get(&(*v1,0)).unwrap().get(&starting_pos).unwrap() + distances.get(&(*v1,l-1)).unwrap().get(&(i,j)).unwrap() + 1)
                    .min()
                    .unwrap();
                result += nb_strait(nb_steps, min_dist_west, l, is_even);

                // NW
                let min_dist_nw = distances.get(&(0,0)).unwrap().get(&starting_pos).unwrap() + distances.get(&(l-1,l-1)).unwrap().get(&(i,j)).unwrap() + 2;
                result += nb_diag(nb_steps, min_dist_nw, l, is_even);

                // NE
                let min_dist_ne = distances.get(&(0,l-1)).unwrap().get(&starting_pos).unwrap() + distances.get(&(l-1,0)).unwrap().get(&(i,j)).unwrap() + 2;
                result += nb_diag(nb_steps, min_dist_ne, l, is_even);

                // SW
                let min_dist_sw = distances.get(&(l-1,0)).unwrap().get(&starting_pos).unwrap() + distances.get(&(0,l-1)).unwrap().get(&(i,j)).unwrap() + 2;
                result += nb_diag(nb_steps, min_dist_sw, l, is_even);

                // SE
                let min_dist_se = distances.get(&(l-1,l-1)).unwrap().get(&starting_pos).unwrap() + distances.get(&(0,0)).unwrap().get(&(i,j)).unwrap() + 2;
                result += nb_diag(nb_steps, min_dist_se, l, is_even);
            }
        }
    }


    println!("Part2: {:?}", (nb_steps, result));
}

fn vertical_corridors(map : &Vec<Vec<char>>) -> Vec<usize> {
    let mut result = Vec::new();
    for j in 0..map[0].len() {
        if map.iter().all(|c| c[j] != '#') {
            result.push(j);
        } 
    }
    result
}

fn horizontal_corridors(map : &Vec<Vec<char>>) -> Vec<usize> {
    let mut result = Vec::new();
    for i in 0..map.len() {
        if map[i].iter().all(|c| *c != '#') {
            result.push(i);
        } 
    }
    result
}

fn distances_from_corridors(map : &Vec<Vec<char>>, v_c : &Vec<usize>, h_c : &Vec<usize>) -> HashMap<(usize, usize), HashMap<(usize, usize), usize>> {
    let mut result = HashMap::new();
    let h = map.len();
    let w = map[0].len();

    for corr in v_c {
        result.insert((0, *corr), distances_from_point(&map, &(0, *corr)));
        result.insert((h-1, *corr), distances_from_point(&map, &(h-1, *corr)));
    }

    for corr in h_c {
        result.insert((*corr, 0), distances_from_point(&map, &(*corr, 0)));
        result.insert((*corr, w-1), distances_from_point(&map, &(*corr, w-1)));
    }

    result
}


fn distances_from_point(map : &Vec<Vec<char>>, c : &(usize, usize)) -> HashMap<(usize, usize), usize> {
    let h = map.len();
    let w = map[0].len();

    let mut dests = HashMap::new();
    dests.insert(c.clone(), 0);
    let mut new_dests = Vec::new();
    new_dests.push(c.clone());
    let mut step = 0;
    loop {
        step += 1;
        let mut temp = Vec::new();
        for d in new_dests {
            let y = d.0;
            let x = d.1;
            if d.0 > 0 && map[y-1][x] != '#' && !dests.contains_key(&(d.0-1, d.1)) {
                temp.push((d.0-1, d.1));
                dests.insert((d.0-1, d.1), step);
            }
            if d.0 < h - 1 && map[y+1][x] != '#' && !dests.contains_key(&(d.0+1, d.1)) {
                temp.push((d.0+1, d.1));
                dests.insert((d.0+1, d.1), step);
            }
            if d.1 > 0 && map[y][x-1] != '#' && !dests.contains_key(&(d.0, d.1-1)) {
                temp.push((d.0, d.1-1));
                dests.insert((d.0, d.1-1), step);
            }
            if d.1 < w - 1 && map[y][x+1] != '#' && !dests.contains_key(&(d.0, d.1+1)) {
                temp.push((d.0, d.1+1));
                dests.insert((d.0, d.1+1), step);
            }
        }
        if temp.len() == 0 {
            break;
        }
        new_dests = temp;
    }
    dests
}

fn nb_strait(max_step: usize, min_dist: usize, l: usize, is_even: bool) -> usize {
    let is_even_inner = (is_even && max_step % 2 == 0) || (!is_even && max_step % 2 == 1);

    let mut nb_map = (max_step - min_dist) / l + 1;
    if (nb_map % 2 == 0 && !is_even_inner) || (nb_map % 2 == 1 && is_even_inner) {
        nb_map -= 1;
    }

    if nb_map % 2 == 1 {
        (nb_map + 1) / 2
    } else {
        nb_map / 2
    }
}

fn nb_diag(max_step: usize, min_dist: usize, l: usize, is_even: bool) -> usize {
    let is_even_inner = (is_even && max_step % 2 == 0) || (!is_even && max_step % 2 == 1);
    
    let mut nb_map = (max_step - min_dist) / l + 1;
    if (nb_map % 2 == 0 && is_even_inner) || (nb_map % 2 == 1 && !is_even_inner) {
        nb_map -= 1;
    }

    if nb_map % 2 == 1 {
        (nb_map + 1)*(nb_map + 1) / 4
    } else {
        (nb_map + 2)* nb_map / 4
    }
}
