use std::fs;

fn main() {
    let file_path = "input_test.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {

    let hailstones = contents.lines()
        .map(|l| -> HailStone {
            let [coord,speed] = l.split('@').collect::<Vec<&str>>()[..] else { panic!("Unexpected input!"); };
            let [x, y, z] = coord.split(',').map(|i| i.trim().parse::<f64>().unwrap()).collect::<Vec<f64>>()[..] else { panic!("Unexpected input!"); };
            let [vx, vy, vz] = speed.split(',').map(|i| i.trim().parse::<f64>().unwrap()).collect::<Vec<f64>>()[..] else { panic!("Unexpected input!"); };
            HailStone {
                x: x,
                y: y,
                z: z,
                vx: vx,
                vy: vy,
                vz: vz
            }
        })
        .collect::<Vec<HailStone>>();

    let min_c = 200000000000000.0;
    let max_c = 400000000000000.0;

    let mut result = 0;
    for i in 0..hailstones.len() {
        for j in i+1..hailstones.len() {
            let s1 = &hailstones[i];
            let s2 = &hailstones[j];

            let coeff = s1.vy*s2.vx - s2.vy*s1.vx;
            if coeff != 0.0
            {
                let x = ((s2.y - s1.y)*s1.vx*s2.vx + s1.x*s1.vy*s2.vx - s2.x*s2.vy*s1.vx) / coeff;

                let t1 = (x - s1.x) / s1.vx;
                let t2 = (x - s2.x) / s2.vx;

                if x > min_c && x < max_c && t1 > 0.0 && t2 > 0.0 {

                    let y = (((s1.y*s1.vx - s1.x*s1.vy) as f64) + (s1.vy as f64)*x) / (s1.vx as f64);
                    if y > min_c && y < max_c {

                        result += 1;
                    }
                }
            }
        }
    }

    println!("Part1: {:?}", result);
}

fn part2(contents: &str) {
    let hailstones = contents.lines()
        .map(|l| -> HailStone {
            let [coord,speed] = l.split('@').collect::<Vec<&str>>()[..] else { panic!("Unexpected input!"); };
            let [x, y, z] = coord.split(',').map(|i| i.trim().parse::<f64>().unwrap()).collect::<Vec<f64>>()[..] else { panic!("Unexpected input!"); };
            let [vx, vy, vz] = speed.split(',').map(|i| i.trim().parse::<f64>().unwrap()).collect::<Vec<f64>>()[..] else { panic!("Unexpected input!"); };
            HailStone {
                x: x,
                y: y,
                z: z,
                vx: vx,
                vy: vy,
                vz: vz
            }
        })
        .collect::<Vec<HailStone>>();


    let w = vec![-3.0, 1.0, 2.0];

    let possible_vx = (-10..10).map(|x| x as f64).filter(|x| hailstones.iter().all(|h| h.vx != *x)).collect::<Vec<f64>>();

    println!("vxs: {:?}", possible_vx);

    let mut matrix = hailstones.iter().map(|h| vec![h.vy-w[1], w[0]-h.vx, (h.vy-w[1])*h.x - (h.vx-w[0])*h.y]).collect::<Vec<Vec<f64>>>();

    println!("Part2: {:?}", gaussian_elimination(&mut matrix));
}

#[derive(Debug)]
struct HailStone {
    x: f64,
    y: f64,
    z: f64,
    vx: f64,
    vy: f64,
    vz: f64
}

#[derive(Debug, Clone)]
struct NoSolution;

fn gaussian_elimination(mut matrix: &mut Vec<Vec<f64>>) -> Result<Vec<f64>, NoSolution>
{
    let nb_variables = matrix[0].len() - 1;

    for j in 0..nb_variables {
        let mut first_row = || -> Result<(), NoSolution> {
            if matrix[j][j] == 0.0 {
                let mut i = 1;
                while i < matrix.len() && matrix[i][j] == 0.0 {
                    i += 1;
                }
                if i == matrix.len() {
                    Err(NoSolution)
                } else {
                    for k in 0..matrix[j].len() {
                        let temp = matrix[i][k];
                        matrix[i][k] = matrix[j][k];
                        matrix[j][k] = temp;
                    }
                    let coeff = matrix[j][j];
                    for k in 0..matrix[j].len() {
                        matrix[j][k] /= coeff;
                    }
                    Ok(())
                }
            } else {
                let coeff = matrix[j][j];
                for k in 0..matrix[j].len() {
                    matrix[j][k] /= coeff;
                }
                Ok(())
            }
        };
        first_row()?;
        for i in j+1..matrix.len() {
            let coeff = matrix[i][j];
            for k in 0..matrix[i].len() {
                matrix[i][k] -= coeff*matrix[j][k];
            }
        }
    }

    let verify_solution = || -> Result<(), NoSolution> {
        if matrix.iter().skip(nb_variables).any(|row| row[nb_variables] != 0.0) {
            Err(NoSolution)
        } else {
            Ok(())
        }
    };
    verify_solution()?;

    for j in 0..nb_variables {
        let col_idx = nb_variables-j-1;

        for i in 0..col_idx {
            let coeff = matrix[i][col_idx];
            for k in 0..matrix[i].len() {
                matrix[i][k] -= coeff*matrix[col_idx][k];
            }
        }
    }

    println!("{:?}", matrix);

    Ok(matrix.iter().take(nb_variables).map(|row| row[nb_variables]).collect::<Vec<f64>>())
}