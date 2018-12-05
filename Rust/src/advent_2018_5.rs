use adventlib::read_input_lines;

extern crate adventlib;

use adventlib::*;

static ASCII_LOWER: [char; 26] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
                                    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                                    's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];

fn react(chars: &mut Vec<char>) {
    let mut i = 0;

    loop {
        if i >= chars.len() - 1 {
            break;
        }
        let a = chars[i];
        let b = chars[i + 1];

        let a_lower = a.to_ascii_lowercase();
        let b_lower = b.to_ascii_lowercase();

        if a_lower == b_lower && a != b {
            chars.drain(i..=i+1);
            i = 0;
        } else {
            i += 1;
        }
    }
}

fn main() {
    let mut input: String = read_input_lines("input.txt").iter().next().unwrap().clone();
    let mut chars: Vec<char> = input.chars().collect();

    let mut p1_chars = chars.clone();

    react(&mut p1_chars);
    println!("Part 1: {}", p1_chars.len());

    let mut results = Vec::new();
    for char_to_remove in ASCII_LOWER.iter().cloned() {
        let char_to_remove_upper = char_to_remove.to_ascii_uppercase();

        let mut chars_removed: Vec<char> = chars.iter().cloned().filter(|c| *c != char_to_remove_upper && *c != char_to_remove).collect();
        react(&mut chars_removed);
        results.push(chars_removed.len());
    }

    let result = results.iter().min().unwrap();
    println!("Part 2: {}", result);
}
