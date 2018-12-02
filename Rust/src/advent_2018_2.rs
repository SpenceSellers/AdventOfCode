use std::hash::Hash;
use std::collections::HashMap;
use std::fmt::Debug;
extern crate itertools;
use itertools::Itertools;
use adventlib::read_input;

extern crate adventlib;

fn histogram<T>(items: &[T]) -> HashMap<&T, usize>
    where T: Hash + Eq + Debug {
    let mut counts = HashMap::new();
    for item in items {
        counts.entry(item).and_modify(|e| *e += 1).or_insert(1);
    }
    return counts;
}

fn has_entries_that_appear_n_times<T: Hash + Eq>(hist: &HashMap<&T, usize>, n: usize) -> bool {
    hist.iter().any(|(_key, val)| *val == n)
}

fn part_1() {
    let chars: Vec<Vec<char>> = read_input("input.txt").iter()
        .map(|l| l.chars().collect())
        .collect();

    let histograms: Vec<HashMap<&char, usize>> = chars.iter()
        .map(|l| histogram(l))
        .collect();

    let num_of = |n: usize| {
        histograms.iter()
            .filter(|hist| has_entries_that_appear_n_times(hist, n))
            .count()
    };

    let num_of_2 = num_of(2);
    let num_of_3 = num_of(3);

    println!("{} * {} = {}", num_of_2, num_of_3, num_of_2 * num_of_3);
}

fn dissimilar_chars(a: &[char], b: &[char]) -> usize {
    a.iter().zip(b).filter(|(aa, bb)| aa != bb).count()
}

fn part_2() {
    let chars: Vec<Vec<char>> = read_input("input.txt")
        .into_iter()
        .map(|l| l.chars().collect())
        .collect();
    let (box_a,box_b)  = chars.into_iter()
        .combinations(2)
        .map(|combo| (combo[0].clone(), combo[1].clone()))
        .find(|(a, b)| dissimilar_chars(&a, &b) == 1)
        .expect("There are no boxes that differ by a single character");

    let a_string: String = box_a.iter().collect();
    let b_string: String = box_b.iter().collect();
    println!("{}", a_string);
    println!("{}", b_string);
}

fn main() {
    part_1();
    part_2();
}
