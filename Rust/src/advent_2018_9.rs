#![feature(euclidean_division)]
extern crate adventlib;

use std::collections::VecDeque;

#[derive(Debug)]
struct MarbleCircle {
    marbles: VecDeque<u64>,
    current_marble_index: usize
}

impl MarbleCircle {
    fn initial_state() -> Self {
        let mut vec = VecDeque::with_capacity(10_000_000);
        vec.push_front(0);
        MarbleCircle {
            marbles: vec,
            current_marble_index: 0
        }
    }

    fn reindex(&self, index: isize) -> usize {
        index.checked_mod_euc(self.marbles.len() as isize).unwrap() as usize
    }

    fn reindex_relative(&self, rel_index: isize) -> usize {
        self.reindex(rel_index + (self.current_marble_index as isize))
    }

    fn insert_relative(&mut self, marble: u64, rel_index: isize) {
        let index = self.reindex_relative(rel_index);
        if index < self.current_marble_index {
            self.current_marble_index += 1;
        }
        self.marbles.insert(index, marble);
    }

    fn remove_relative(&mut self, rel_index: isize) -> u64 {
        let index = self.reindex_relative(rel_index);
        if index < self.current_marble_index {
            self.current_marble_index -= 1;
        }
        return self.marbles.remove(index).unwrap();
    }

    fn get_relative(&self, rel_index: isize) -> u64 {
        let index = self.reindex_relative(rel_index);
        return self.marbles[index];
    }

    fn shift_current(&mut self, rel_index: isize) {
        self.current_marble_index = self.reindex_relative(rel_index);
    }

    /// This is the secret sauce of this solution.
    /// Every once in a while, shift our current position so that the current marble is
    /// close to one end of our double-ended vector. That way, inserts and removals only have to
    /// move a few dozen elements instead of zillions.
    fn re_rack(&mut self) {
        for i in 0..self.current_marble_index {
            let v = self.marbles.pop_front().expect("Empty on re-rack");
            self.marbles.push_back(v);
        }
        self.current_marble_index = 0;
    }
}

fn main() {
    const PLAYERS: usize = 413;
    const LAST_POINTS: u64 = 7108200;
//    const PLAYERS: usize = 9;
//    const LAST_POINTS: u64 = 25;

    let mut circle = MarbleCircle::initial_state();
    let mut scores = vec![0; PLAYERS];

    let mut next_marble = 1;
    let mut current_player = 0;

    while next_marble <= LAST_POINTS {
        if next_marble % 50 == 0 {
            circle.re_rack();
        }
        if next_marble % 23 == 0 {
            // Player gets the score of the marble
            scores[current_player] += next_marble;
            let next_current = circle.get_relative(-6);
            let removed = circle.remove_relative(-7);
            circle.shift_current(-6);
            // And the score of the one removed.
            scores[current_player] += removed;

        } else {
            circle.insert_relative(next_marble, 2);
            circle.shift_current(2);
        }

        next_marble += 1;

        current_player += 1;
        current_player %= PLAYERS;
    }

    let max_score = scores.iter().max().unwrap();
    println!("Max score: {}", max_score);
}