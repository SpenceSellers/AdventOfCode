use std::collections::HashSet;
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

fn tick(state: &HashSet<isize>, buffer: &mut HashSet<isize>, rules: &[Rule<bool>]) {
    buffer.clear();
    let mut check_set: HashSet<isize> = HashSet::new();

    for i in state.iter() {
        for di in (-4)..=(4) {
            check_set.insert(di + *i);
        }
    }

    for i in check_set {
        let res = apply_rule(i, state, rules);
        if res {
            buffer.insert(i);
        }
    }
}

fn apply_rule(index: isize, state: &HashSet<isize>, rules: &[Rule<bool>]) -> bool {
    fn get(i: isize, state: &HashSet<isize>) -> bool {
        state.contains(&i)
    };

    for rule in rules {
        let aa = [
            get(index - 2, state),
            get(index - 1, state),
            get(index + 0, state),
            get(index + 1, state),
            get(index + 2, state),
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
    let mut state: HashSet<isize> = HashSet::new();
    let mut buffer: HashSet<isize> = HashSet::new();

    for (i, s) in initial.iter().enumerate() {
        if *s {
            state.insert(i as isize);
        }
    }

    for gen in 0..200_000 {
        if gen % 50_000 == 0 {
            println!("Generation {}", gen);
            println!("cells: {}", state.len());
            let result: isize = state.iter().map(|k| *k).sum();
            println!("res: {}", result);
        }
        tick(&state, &mut buffer, &rules);
        std::mem::swap(&mut state, &mut buffer);
    }

    println!("And now use your human brain to deduce the answer");
    // // P1
    // let result: isize = state.iter().map(|k| *k).sum();
    // println!("{}", result);
}