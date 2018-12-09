#![feature(euclidean_division)]
extern crate adventlib;

#[derive(Debug)]
struct MarbleCircle {
    marbles: Vec<u64>,
    current_marble_index: usize
}

impl MarbleCircle {
    fn initial_state() -> Self {
        MarbleCircle {
            marbles: vec![0],
            current_marble_index: 0
        }
    }

    fn current_marble_index(&self) -> usize {
//        self.marbles.iter()
//            .position(|marble| *marble == self.current_marble)
//            .expect("Current marble is not in circle. This shouldn't happen.")
        self.current_marble_index
    }

    fn reindex(&self, index: isize) -> usize {
        index.checked_mod_euc(self.marbles.len() as isize).unwrap() as usize
    }

    fn reindex_relative(&self, rel_index: isize) -> usize {
        self.reindex(rel_index + (self.current_marble_index() as isize))
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
        return self.marbles.remove(index)
    }

    fn get_relative(&self, rel_index: isize) -> u64 {
        let index = self.reindex_relative(rel_index);
        return self.marbles[index];
    }

    fn shift_current(&mut self, rel_index: isize) {
        self.current_marble_index = self.reindex_relative(rel_index);
    }
}

fn main() {
    const PLAYERS: usize = 413;
    const LAST_POINTS: u64 = 71082;
//    const PLAYERS: usize = 9;
//    const LAST_POINTS: u64 = 25;

    let mut circle = MarbleCircle::initial_state();
    let mut scores = vec![0; PLAYERS];

    let mut next_marble = 1;
    let mut current_player = 0;

    while next_marble <= LAST_POINTS {
        if next_marble % 100000 == 0 {
            println!("At: {}", next_marble);
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