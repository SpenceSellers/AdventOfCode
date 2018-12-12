use std::collections::VecDeque;
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

fn tick(state: &[bool], rules: &[Rule<bool>]) -> Vec<bool> {
    let mut next = Vec::new();

    for i in (-3 as isize)..(state.len() as isize + 3) {
        next.push(apply_rule(i, state, rules));
    }
    return next;
}

fn apply_rule(index: isize, state: &[bool], rules: &[Rule<bool>]) -> bool {
    let get = |i| { 
        if i >= 0 {
            state.get(i as usize).cloned().unwrap_or(false)
        } else {
            false
        }
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

    let mut state: Vec<bool> = initial.into_iter().collect();
    for _ in 0..20 {
        state = tick(&state, &rules);
    }

    println!("{}", state.iter().cloned().filter(|c| *c).count());

    println!("{:?}", state);
}