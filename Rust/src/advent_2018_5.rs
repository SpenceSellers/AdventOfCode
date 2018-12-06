use adventlib::read_input_lines;
use std::time::Instant;

extern crate adventlib;

use adventlib::*;

fn lowercase(x: u8) -> u8 {
    x | 0b0010_0000
}

fn uppercase(x: u8) -> u8 {
    x & 0b1101_1111
}

fn react(chars: &mut Vec<u8>, buffer: &mut Vec<u8>) {
    debug_assert!(chars.len() > 2);
    debug_assert!(buffer.capacity() >= chars.len());

    let mut i = 0;
    buffer.clear();

    let mut found_any = false;
    loop {
        // We check one character ahead, so we don't want to run up to the very end
        if i >= chars.len() - 1 {
            if i != chars.len() {
                // The last character can't cause a reaction that the one before didn't catch.
                unsafe {
                    let last_char: u8 = *chars.get_unchecked(i);
                    unchecked_push(buffer, last_char)
                };
            };
            if !found_any { break; }
            found_any = false;
            std::mem::swap(chars, buffer);
            buffer.clear();
            i = 0;
        }

        let a = unsafe { *chars.get_unchecked(i) };
        let b = unsafe { *chars.get_unchecked(i + 1) };

        let a_lower = lowercase(a);
        let b_lower = lowercase(b);

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
unsafe fn unchecked_push<T>(buf: &mut Vec<T>, new: T) {
    let old_len = buf.len();
    *buf.get_unchecked_mut(old_len) = new;
    buf.set_len(old_len + 1);
}

fn main() {
    let now = Instant::now();
    let mut input: String = read_input_lines("input.txt").iter().next().unwrap().clone();
    let mut chars: Vec<u8> = input.bytes().collect();

    let mut p1_chars = chars.clone();
    let mut buffer = Vec::with_capacity(chars.len());

    react(&mut p1_chars, &mut buffer);
    println!("Part 1: {}", p1_chars.len());

    let mut results = Vec::with_capacity(26);
    let mut chars_removed: Vec<u8> = Vec::with_capacity(chars.len());

    for char_to_remove in 97..=122 {
        let char_to_remove_upper = uppercase(char_to_remove);

        chars_removed.clear();
        chars_removed.extend(p1_chars.iter().cloned().filter(|c| *c != char_to_remove_upper && *c != char_to_remove));

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

mod test {
    #[test]
    fn uppercase() {
        // 'a' 'A'
        assert_eq!(super::uppercase(97), 65);
        // 'y' 'Y'
        assert_eq!(super::uppercase(121), 89);
    }

    #[test]
    fn lowercase() {
        // 'A' 'a'
        assert_eq!(super::lowercase(65), 97);
        // 'Y' 'y'
        assert_eq!(super::uppercase(89), 121);
    }
}