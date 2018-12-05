use adventlib::read_input_lines;
use std::time::Instant;

extern crate adventlib;

use adventlib::*;

static ASCII_LOWER: [char; 26] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
                                    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                                    's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];

fn react(chars: &mut Vec<char>, buffer: &mut Vec<char>) {
    let mut i = 0;
    buffer.clear();

    let mut found_any = false;
    loop {
        if i >= chars.len() - 1 {
            if i != chars.len() { 
                // The last character can't cause a reaction that the one before didn't catch.
                unchecked_push(buffer, chars[i]); 
            };
            if !found_any { break; }
            found_any = false;
            std::mem::swap(chars, buffer);
            buffer.clear();
            i = 0;
        }

        let a = unsafe { *chars.get_unchecked(i) };
        let b = unsafe { *chars.get_unchecked(i + 1) };

        let a_lower = a.to_ascii_lowercase();
        let b_lower = b.to_ascii_lowercase();

        if a_lower == b_lower && a != b {
            found_any = true;
            i += 2;
        } else {
            unsafe { unchecked_push(buffer, a) };
            i += 1;
        }
    }
}

// Will push an element onto the end of a vector, without checking if 
// the vector needs to allocate more room. Use your imagination for what will 
// happen if you use this wrong.
fn unchecked_push<T>(buf: &mut Vec<T>, new: T) {
    let old_len = buf.len();
    unsafe {
        *buf.get_unchecked_mut(old_len) = new;
        buf.set_len(old_len + 1);
    }
}

fn main() {
    let now = Instant::now();
    let mut input: String = read_input_lines("input.txt").iter().next().unwrap().clone();
    let mut chars: Vec<char> = input.chars().collect();

    let mut p1_chars = chars.clone();
    let mut buffer = Vec::with_capacity(chars.len());

    react(&mut p1_chars, &mut buffer);
    println!("Part 1: {}", p1_chars.len());

    let mut results = Vec::with_capacity(26);
    let mut chars_removed: Vec<char> = Vec::with_capacity(chars.len());

    for char_to_remove in ASCII_LOWER.iter().cloned() {
        let char_to_remove_upper = char_to_remove.to_ascii_uppercase();

        chars_removed.clear();
        chars_removed.extend(chars.iter().cloned().filter(|c| *c != char_to_remove_upper && *c != char_to_remove));

        react(&mut chars_removed, &mut buffer);
        results.push(chars_removed.len());
    }

    let result = results.iter().min().unwrap();
    println!("Part 2: {}", result);
    let duration = now.elapsed();
    println!(
        "Solved in {}.{:09}s",
        duration.as_secs(),
        duration.subsec_nanos()
    );
}
