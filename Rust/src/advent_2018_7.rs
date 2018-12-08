#![feature(drain_filter)] 
use regex::Regex;
use std::collections::BTreeSet;
use adventlib::AsciiValue;
use std::collections::HashMap;

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

struct Dependencies {
    requirements: HashMap<char, BTreeSet<char>>
}

impl Dependencies {
    fn from_deps(deps: &[(char, char)]) -> Self {
        let mut ds = Dependencies {
            requirements: HashMap::new()
        };

        for (step, requires) in deps.iter().cloned() {
            ds.add_requirement(step, requires);
        }

        return ds;
    }

    fn add_requirement(&mut self, step: char, requires: char) {
        self.requirements.entry(step)
            .and_modify(|reqset| {
                reqset.insert(requires);
            })
            .or_insert_with(|| {
                let mut set = BTreeSet::new();
                set.insert(requires);
                return set;
            });
    }

    fn all_steps(&self) -> BTreeSet<char> {
        let mut all = BTreeSet::new();
        for step in self.requirements.keys() {
            all.insert(*step);
            for req in self.requirements.get(step).unwrap() {
                all.insert(*req);
            }
        }
        return all;
    }

    fn is_ready_to_run(&self, step: char, completed: &BTreeSet<char>) -> bool {
        let requirements = self.requirements.get(&step);
        if let Some(requirements) = requirements {
            for requires in requirements.iter() {
                if !completed.contains(requires) {
                    return false;
                }
            }
        }
        return true;
}

    fn ready_to_run(&self, completed: &BTreeSet<char>) -> BTreeSet<char> {
        let remaining_steps: BTreeSet<char> = self.all_steps().difference(completed).cloned().collect();
        let mut ready_to_run = BTreeSet::new();
        for step in remaining_steps {
            if self.is_ready_to_run(step, completed) {
                ready_to_run.insert(step);
            }
        }

        return ready_to_run;
    }
}

fn p1(deps: &Dependencies) {
    let mut answer = String::new();
    let mut completed: BTreeSet<char> = BTreeSet::new();

    loop {
        let no_deps = deps.ready_to_run(&completed);
        let next = no_deps.iter().next();
        match next {
            Some(next) => {
                answer.push(*next);
                completed.insert(*next);
            }
            None => {
                break;
            }
        }
    }
    println!("Should be:\nCFGHAEMNBPRDISVWQUZJYTKLOX");
    println!("{}", answer);
}

fn get_time_to_complete(c: char, multiplier: u64) -> u64 {
    AsciiValue::from_char(c).unwrap().uppercase_index().unwrap() as u64 * multiplier
}

fn p2(deps: &Dependencies) {
    const MAX_ELVES: usize = 2;

    let mut completed: BTreeSet<char> = BTreeSet::new();
    let mut elves: Vec<Option<(char, u64)>> = Vec::new();

    loop {
        let no_deps = deps.ready_to_run(&completed);
        let next = no_deps.iter().next();
        match next {
            Some(next) => {
                answer.push(*next);
                completed.insert(*next);
            }
            None => {
                if elves.is_empty() {
                    // Elves are done working and we have nothing left to do
                    break;
                }
                // Else wait for elves to finish what they're doing.
            }
        }
    }
}

fn main() {
    let dependencies: Vec<(char, char)> = adventlib::read_input_lines("input.txt").iter()
        .map(|line| parse_dependency(line))
        .collect();

    let dependencies = Dependencies::from_deps(&dependencies);

    p1(&dependencies);
    p2(&dependencies);
}