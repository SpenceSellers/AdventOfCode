use std::fs::File;
use std::io::prelude::*;
use std::collections::HashSet;
use std::hash::Hash;

fn read_nums() -> Vec<i32> {
    let mut file = File::open("input.txt").expect("Could not find input file");
    let mut string = String::new();
    file.read_to_string(&mut string).expect("Could not read file");

    return string.lines()
        .map(|line| line.parse().expect("Line is not a valid integer"))
        .collect();
}

fn endless_sum<'a>(nums: &'a [i32]) -> impl Iterator<Item=i32> + 'a {
    nums.iter().cycle().scan(0, |state, &num| {
        *state += num;
        Some(*state)
    })
}

fn first_repeat<T>(items: impl Iterator<Item=T>) -> Option<T>
    where T: Eq + Hash {
    let mut seen = HashSet::new();
    for num in items {
        if seen.contains(&num) {
            return Some(num);
        }
        seen.insert(num);
    }
    return None;
}

fn main() {
    let nums = read_nums();
    let result = first_repeat(endless_sum(&nums)).unwrap();
    println!("First repeat: {}", result);
}
