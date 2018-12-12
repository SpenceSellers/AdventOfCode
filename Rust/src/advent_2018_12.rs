use std::collections::HashMap;
extern crate adventlib;

#[derive(Debug)]
struct Rule<T> {
    cond: Vec<T>,
    becomes: T
}

fn parse_rule(s: &str) -> Rule<bool> {
    let pieces: Vec<_> = s.split_whitespace().collect();
    let cond = pieces[0].chars().map(|c| c == '#').collect();
    let result = pieces[2] == "#";
    Rule {
        cond,
        becomes: result
    }
}

fn parse_initial_state(s: &str) -> Vec<bool> {
    let state_str = s.split_whitespace().skip(2).next().expect("Unreadable state string");
    state_str.chars().map(|c| c == '#').collect()
}

fn tick(state: &HashMap<isize, bool>, rules: &[Rule<bool>]) -> HashMap<isize, bool> {
    let mut next = HashMap::new();

    let min = state.keys().min().unwrap();
    let max = state.keys().max().unwrap();

    for i in (min - 5)..(max + 5) {
        let res = apply_rule(i, state, rules);
        if res {
            next.insert(i, true);
        }
    }
    return next;
}

fn apply_rule(index: isize, state: &HashMap<isize, bool>, rules: &[Rule<bool>]) -> bool {
    let get = |i: isize| { 
        state.get(&i).cloned().unwrap_or(false)
    };

    for rule in rules {
        let aa = [
            get(index - 2),
            get(index - 1),
            get(index + 0),
            get(index + 1),
            get(index + 2),
        ];
        if rule.cond == aa {
            return rule.becomes;
        }
    }
    panic!("We have an incomplete set of rules");
}

fn main() {
    println!("Starting");
    let input = adventlib::read_input_lines("input.txt");
    let initial = parse_initial_state(&input[0]);
    let rules: Vec<_> = input[2..].iter().map(|line| parse_rule(line)).collect();
    let mut state: HashMap<isize, bool> = HashMap::new();

    for (i, s) in initial.iter().enumerate() {
        state.insert(i as isize, *s);
    }

    for gen in 0..500_000 {
        if gen % 100_000 == 0 {
            println!("Generation {}", gen);
        }
        state = tick(&state, &rules);
    }

    let result: isize = state.iter().filter(|(_k, v)| **v).map(|(k, _v)| *k).sum();
    println!("{}", result);

    println!("{:?}", state);
}