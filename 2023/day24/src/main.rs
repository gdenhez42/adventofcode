use std::fs;
use float_cmp::approx_eq;

type D = fraction::GenericFraction<fraction::BigInt>;

fn main() {
    let file_path = "input.txt";

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) {

    let hailstones = contents.lines()
        .map(|l| -> HailStone {
            let [coord,speed] = l.split('@').collect::<Vec<&str>>()[..] else { panic!("Unexpected input!"); };
            let [x, y, _] = coord.split(',').map(|i| i.trim().parse::<f64>().unwrap()).collect::<Vec<f64>>()[..] else { panic!("Unexpected input!"); };
            let [vx, vy, _] = speed.split(',').map(|i| i.trim().parse::<f64>().unwrap()).collect::<Vec<f64>>()[..] else { panic!("Unexpected input!"); };
            HailStone {
                x: x,
                y: y,
                vx: vx,
                vy: vy
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

                    let y = ((s1.y*s1.vx - s1.x*s1.vy) + s1.vy*x) / s1.vx;
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
        .map(|l| -> HailStonePt2 {
            let [coord,speed] = l.split('@').collect::<Vec<&str>>()[..] else { panic!("Unexpected input!"); };
            let [x, y, z] = coord.split(',').map(|i| i.trim().parse::<i64>().unwrap()).collect::<Vec<i64>>()[..] else { panic!("Unexpected input!"); };
            let [vx, vy, vz] = speed.split(',').map(|i| i.trim().parse::<i64>().unwrap()).collect::<Vec<i64>>()[..] else { panic!("Unexpected input!"); };
            HailStonePt2 {
                x: x,
                y: y,
                z: z,
                vx: vx,
                vy: vy,
                vz: vz
            }
        })
        .collect::<Vec<HailStonePt2>>();

    let mut vx = -1000;
    let mut vy;
    'vx: loop {
        if vx % 100 == 0 {
            println!("{:?}", vx);
        }
        vy = -1000;
        'vy: loop {
            let wx = vx as f64;
            let wy = vy as f64;
            let mut matrix = hailstones.iter().take(10).map(|h| vec![(h.vy as f64)-wy, wx-(h.vx as f64), ((h.vy as f64)-wy)*(h.x as f64) - ((h.vx as f64)-wx)*(h.y as f64)]).collect::<Vec<Vec<f64>>>();
            match check_solution(&mut matrix, 10000.0) {
                Ok(_) => { 
                    break 'vx
                }
                Err(_) => {}
            }
            vy += 1;
            if vy == 1000 {
                break 'vy
            }
        }
        vx += 1;
        if vx == 1000 {
            panic!("Did not find a solution")
        }
    }

    let mut vz = -1000;
    loop {
        let wz = vz as f64;
        let wy = vy as f64;
        let mut matrix = hailstones.iter().take(10).map(|h| vec![(h.vy as f64)-wy, wz-(h.vz as f64), ((h.vy as f64)-wy)*(h.z as f64) - ((h.vz as f64)-wz)*(h.y as f64)]).collect::<Vec<Vec<f64>>>();
        match check_solution(&mut matrix, 10000.0) {
            Ok(_) => { 
                break
            }
            Err(_) => {}
        }
        vz += 1;
        if vz == 1000 {
            panic!("Did not find a solution")
        }
    }

    println!("{:?}, {:?}, {:?}", vx, vy, vz );
    

    let wx = D::from(vx);
    let wy = D::from(vy);
    let wz = D::from(vz);
    let mut matrix_xy = hailstones.iter().take(5).map(|h| vec![D::from(h.vy)-wy.clone(), wx.clone()-D::from(h.vx), (D::from(h.vy)-wy.clone())*D::from(h.x) - (D::from(h.vx)-wx.clone())*D::from(h.y)]).collect::<Vec<Vec<D>>>();
    let result_xy = gaussian_elimination(&mut matrix_xy).expect("Should be solved with the above bruteforce");

    let mut matrix_yz = hailstones.iter().take(5).map(|h| vec![D::from(h.vy)-wy.clone(), wz.clone()-D::from(h.vz), (D::from(h.vy)-wy.clone())*D::from(h.z) - (D::from(h.vz)-wz.clone())*D::from(h.y)]).collect::<Vec<Vec<D>>>();
    let result_yz = gaussian_elimination(&mut matrix_yz).expect("Should be solved with the above bruteforce");

    println!("[{:.4}, {:.4}, {:.4}]", result_xy[0], result_xy[1], result_yz[0]);

    println!("part2: {:.4}", result_xy[0].clone() + result_xy[1].clone() + result_yz[0].clone());
}

#[derive(Debug)]
struct HailStone {
    x: f64,
    y: f64,
    vx: f64,
    vy: f64,
}

#[derive(Debug)]
struct HailStonePt2 {
    x: i64,
    y: i64,
    z: i64,
    vx: i64,
    vy: i64,
    vz: i64
}

#[derive(Debug, Clone)]
struct NoSolution;

fn gaussian_elimination(matrix: &mut Vec<Vec<D>>) -> Result<Vec<D>, NoSolution>
{
    let nb_variables = matrix[0].len() - 1;
    let zero = D::from(0);

    /*print!("[");
    for i in 0..matrix.len() {
        print!("[");
        for j in 0..matrix[i].len() {
            print!("{:.4}, ", matrix[i][j]);
        }
        print!("]");
    }
    println!("]");*/

    for j in 0..nb_variables {
        let mut first_row = || -> Result<(), NoSolution> {
            if matrix[j][j] == zero {
                let mut i = 1;
                while i < matrix.len() && matrix[i][j] == zero {
                    i += 1;
                }
                if i == matrix.len() {
                    Err(NoSolution)
                } else {
                    for k in 0..matrix[j].len() {
                        let temp = matrix[i][k].clone();
                        matrix[i][k] = matrix[j][k].clone();
                        matrix[j][k] = temp;
                    }
                    let coeff = matrix[j][j].clone();
                    for k in 0..matrix[j].len() {
                        let newval = matrix[j][k].clone() / coeff.clone();
                        matrix[j][k] = newval;
                    }
                    Ok(())
                }
            } else {
                let coeff = matrix[j][j].clone();
                for k in 0..matrix[j].len() {
                    let newval = matrix[j][k].clone() / coeff.clone();
                    matrix[j][k] = newval;
                }
                Ok(())
            }
        };
        first_row()?;
        for i in j+1..matrix.len() {
            let coeff = matrix[i][j].clone();
            for k in 0..matrix[i].len() {
                let newval = coeff.clone()*matrix[j][k].clone();
                matrix[i][k] -= newval;
            }
        }
    }

    /*print!("[");
    for i in 0..matrix.len() {
        print!("[");
        for j in 0..matrix[i].len() {
            print!("{:.4}, ", matrix[i][j]);
        }
        print!("]");
    }
    println!("]");*/


    let verify_solution = || -> Result<(), NoSolution> {
        if matrix.iter().skip(nb_variables).any(|row| row[nb_variables] != zero) {
            Err(NoSolution)
        } else {
            Ok(())
        }
    };
    verify_solution()?;

    for j in 0..nb_variables {
        let col_idx = nb_variables-j-1;

        for i in 0..col_idx {
            let coeff = matrix[i][col_idx].clone();
            for k in 0..matrix[i].len() {
                let newval = coeff.clone()*matrix[col_idx][k].clone();
                matrix[i][k] -= newval;
            }
        }
    }

    Ok(matrix.iter().take(nb_variables).map(|row| row[nb_variables].clone()).collect::<Vec<D>>())
}

// Using fraction is slow as... but f64 lack precision
// So, I use f64 to look for a solution and fraction to get the result
fn check_solution(matrix: &mut Vec<Vec<f64>>, e: f64) -> Result<(), NoSolution>
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
        if matrix.iter().skip(nb_variables).any(|row| !approx_eq!(f64, row[nb_variables], 0.0, epsilon=e)) {
            Err(NoSolution)
        } else {
            Ok(())
        }
    };
    verify_solution()?;

    Ok(())
}