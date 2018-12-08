#![feature(drain_filter)] 
use regex::Regex;
use std::collections::HashSet;
use std::collections::BTreeSet;

extern crate adventlib;
extern crate regex;
#[macro_use] extern crate lazy_static;

fn parse_dependency(s: &str) -> (char, char) {
    lazy_static! {
        static ref REGEX: Regex = regex::Regex::new(r"Step (.) must be finished before step (.) can begin.").unwrap();
    }

    let captures = REGEX.captures(s).expect("Could not parse dependency!");

    let requires: char = captures.get(1).unwrap().as_str().parse().unwrap();
    let step: char = captures.get(2).unwrap().as_str().parse().unwrap();

    return (step, requires);
}

fn steps_waiting_to_run(deps: &[(char, char)]) -> BTreeSet<char> {
    deps.iter().map(|(step, requires)| *step).collect()
}

fn have_no_dependencies(deps: &[(char, char)]) -> BTreeSet<char> {
    let all_steps: BTreeSet<char>  = deps.iter().map(|(step, requires)| *requires).collect();
    let waiting = steps_waiting_to_run(deps);
    return all_steps.difference(&waiting).cloned().collect();
}

fn depends_on_nothing(deps: &[(char, char)]) -> BTreeSet<char> {
    let all_steps: BTreeSet<char> = deps.iter().map(|(step, requires)| *step).collect();
    let all_required: BTreeSet<char> = deps.iter().map(|(step, requires)| *requires).collect();
    return all_steps.difference(&all_required).cloned().collect();
}

fn p1(deps: &[(char, char)]) {
    let mut deps: Vec<(char, char)> = deps.to_vec();
    let final_steps = depends_on_nothing(&deps);
    let mut answer = String::new();

    loop {
        let no_deps = have_no_dependencies(&deps);
        let next = no_deps.iter().next();
        match next {
            Some(next) => {
                answer.push(*next);
                deps.drain_filter(|(step, requires)| requires == next);
            }
            None => {
                break;
            }
        }
    }

    for final_step in final_steps {
        answer.push(final_step);
    }

    println!("{}", answer);
}

fn get_time_to_complete(c: char, multiplier: u64) -> u64 {
    0
}

fn p2(deps: &[(char, char)]) {
    let mut deps: Vec<(char, char)> = deps.to_vec();
    let final_steps = depends_on_nothing(&deps);
    let mut answer = String::new();

    loop {
        let no_deps = have_no_dependencies(&deps);
        let next = no_deps.iter().next();
        match next {
            Some(next) => {
                // Do "next"
                deps.drain_filter(|(step, requires)| requires == next);
            }
            None => {
                break;
            }
        }
    }

    for final_step in final_steps {
        // Do final steps
    }

    println!("{}", answer);
}

fn main() {
    let dependencies: Vec<(char, char)> = adventlib::read_input_lines("input.txt").iter()
        .map(|line| parse_dependency(line))
        .collect();

    p1(&dependencies);
}