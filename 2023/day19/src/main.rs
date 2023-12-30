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
    let [workflows_str, parts_str] = contents.split("\n\n").collect::<Vec<&str>>()[..] else { panic!("unexpected input!") };

    // Parse workflows
    let mut workflows = HashMap::new();
    for line in workflows_str.lines() {
        let [flow_name, flows_str] = line.split('{').collect::<Vec<&str>>()[..] else { panic!("unexpected input!") };
        let flows = flows_str.trim_end_matches('}').split(',')
            .map(|s| -> Rule {
                match s.find(':') {
                    Some(i) => {
                        let mut c = s.chars();
                        Rule::Conditional {
                            composant: c.next().unwrap(),
                            operation: c.next().unwrap(),
                            qty: s[2..i].parse::<u64>().unwrap(),
                            workflow: s[i+1..].to_string()
                        }
                    },
                    None => {
                        Rule::Default(s.to_string())
                    }
                }
            })
            .collect::<Vec<Rule>>();
        workflows.insert(flow_name.to_string(), flows);
    }

    // Parse parts
    let mut parts = Vec::new();
    for line in parts_str.lines() {
        let mut tokens = line.trim_matches(|c| c == '{'  || c == '}')
            .split(',');
        parts.push(Part {
            x: tokens.next().unwrap()[2..].parse::<u64>().unwrap(),
            m: tokens.next().unwrap()[2..].parse::<u64>().unwrap(),
            a: tokens.next().unwrap()[2..].parse::<u64>().unwrap(),
            s: tokens.next().unwrap()[2..].parse::<u64>().unwrap()
        });
    }


    // For each part, apply workflows
    let mut result = 0;
    for part in parts {
        let mut workflow = "in".to_string();
        let mut workflow_idx = 0;
        while workflow != "R" && workflow != "A" {

            match &workflows.get(&workflow).unwrap()[workflow_idx] {
                Rule::Conditional {
                    composant,
                    operation,
                    qty,
                    workflow: rule_workflow
                } => {
                    let part_qty = match composant {
                        'x' => part.x,
                        'm' => part.m,
                        'a' => part.a,
                        's' => part.s,
                        _ => {
                            panic!("Unexpected component!");
                        }
                    };
                    match operation {
                        '>' => {
                            if part_qty > *qty {
                                workflow = rule_workflow.clone();
                                workflow_idx = 0;
                            } else {
                                workflow_idx += 1;
                            }
                        },
                        '<' => {
                            if part_qty < *qty {
                                workflow = rule_workflow.clone();
                                workflow_idx = 0;
                            } else {
                                workflow_idx += 1;
                            }
                        },
                        _ => { panic!("Unexpected operator!"); }
                    }
                },
                Rule::Default(rule) => {
                    workflow = rule.clone();
                    workflow_idx = 0;
                }
            }


        }

        if workflow == "A" {
            result += part.x + part.m + part.a + part.s;
        }
    }

    println!("Part1: {:?}", result);
}

fn part2(contents: &str) {
    let [workflows_str, _] = contents.split("\n\n").collect::<Vec<&str>>()[..] else { panic!("unexpected input!") };

    // Parse workflows
    let mut workflows = HashMap::new();
    for line in workflows_str.lines() {
        let [flow_name, flows_str] = line.split('{').collect::<Vec<&str>>()[..] else { panic!("unexpected input!") };
        let flows = flows_str.trim_end_matches('}').split(',')
            .map(|s| -> Rule {
                match s.find(':') {
                    Some(i) => {
                        let mut c = s.chars();
                        Rule::Conditional {
                            composant: c.next().unwrap(),
                            operation: c.next().unwrap(),
                            qty: s[2..i].parse::<u64>().unwrap(),
                            workflow: s[i+1..].to_string()
                        }
                    },
                    None => {
                        Rule::Default(s.to_string())
                    }
                }
            })
            .collect::<Vec<Rule>>();
        workflows.insert(flow_name.to_string(), flows);
    }
    let part_comb = PartComb {
        x: Range(1,4000),
        m: Range(1,4000),
        a: Range(1,4000),
        s: Range(1,4000)
    };

    println!("Part2: {:?}", calculate_workflow_combinaisons(&part_comb, &"in".to_string(), 0, &workflows));
}

#[derive(Debug)]
struct Part {
    x: u64,
    m: u64,
    a: u64,
    s: u64
}

#[derive(Debug)]
enum Rule {
    Conditional {
        composant: char,
        operation: char,
        qty: u64,
        workflow: String
    },
    Default(String)
}

#[derive(Debug, Clone, Copy)]
struct Range(u64, u64);

#[derive(Debug, Clone, Copy)]
struct PartComb {
    x: Range,
    m: Range,
    a: Range,
    s: Range
}

fn calculate_workflow_combinaisons(part_comb: &PartComb, workflow_name: &String, workflow_idx: usize, workflows: &HashMap<String,Vec<Rule>>) -> u64 {
    if !has_combinaisons_left(part_comb) || workflow_name == "R" {
        0
    } else if workflow_name == "A" {
        (part_comb.x.1-part_comb.x.0+1)*(part_comb.m.1-part_comb.m.0+1)*(part_comb.a.1-part_comb.a.0+1)*(part_comb.s.1-part_comb.s.0+1)
    } else {
        let rule = &workflows.get(workflow_name).unwrap()[workflow_idx];

        match rule {
            Rule::Conditional {
                composant,
                operation,
                qty,
                workflow
            } => {
                let (part_comb_1, part_comb_2) = match (composant, operation) {
                    ('x', '>') => {
                        (PartComb {
                            x : Range(*qty+1, part_comb.x.1),
                            ..*part_comb
                        }, PartComb {
                            x : Range(part_comb.x.0, *qty),
                            ..*part_comb
                        })
                    },
                    ('m', '>') => {
                        (PartComb {
                            m : Range(*qty+1, part_comb.m.1),
                            ..*part_comb
                        }, PartComb {
                            m : Range(part_comb.m.0, *qty),
                            ..*part_comb
                        })
                    },
                    ('a', '>') => {
                        (PartComb {
                            a : Range(*qty+1, part_comb.a.1),
                            ..*part_comb
                        }, PartComb {
                            a : Range(part_comb.a.0, *qty),
                            ..*part_comb
                        })
                    },
                    ('s', '>') => {
                        (PartComb {
                            s : Range(*qty+1, part_comb.s.1),
                            ..*part_comb
                        }, PartComb {
                            s : Range(part_comb.s.0, *qty),
                            ..*part_comb
                        })
                    },
                    ('x', '<') => {
                        (PartComb {
                            x : Range(part_comb.x.0, *qty-1),
                            ..*part_comb
                        }, PartComb {
                            x : Range(*qty, part_comb.x.1),
                            ..*part_comb
                        })
                    },
                    ('m', '<') => {
                        (PartComb {
                            m : Range(part_comb.m.0, *qty-1),
                            ..*part_comb
                        }, PartComb {
                            m : Range(*qty, part_comb.m.1),
                            ..*part_comb
                        })
                    },
                    ('a', '<') => {
                        (PartComb {
                            a : Range(part_comb.a.0, *qty-1),
                            ..*part_comb
                        }, PartComb {
                            a : Range(*qty, part_comb.a.1),
                            ..*part_comb
                        })
                    },
                    ('s', '<') => {
                        (PartComb {
                            s : Range(part_comb.s.0, *qty-1),
                            ..*part_comb
                        }, PartComb {
                            s : Range(*qty, part_comb.s.1),
                            ..*part_comb
                        })
                    },
                    _ => { panic!("Unexpected operator and composant!"); }
                };
                calculate_workflow_combinaisons(&part_comb_1, &workflow, 0, workflows) + calculate_workflow_combinaisons(&part_comb_2, workflow_name, workflow_idx + 1, workflows)
            },
            Rule::Default(workflow) => {
                calculate_workflow_combinaisons(part_comb, &workflow, 0, workflows)
            }
        }
    }
}

fn has_combinaisons_left(part_comb: &PartComb) -> bool {
    return part_comb.x.1 >= part_comb.x.0 &&
        part_comb.m.1 >= part_comb.m.0 &&
        part_comb.a.1 >= part_comb.a.0 &&
        part_comb.s.1 >= part_comb.s.0;
}
