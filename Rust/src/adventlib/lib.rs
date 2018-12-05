use std::fs::File;
use std::io::Read;
use std::hash::Hash;
use std::collections::HashMap;

pub mod masked;

pub fn read_input_lines(filename: &str) -> Vec<String> {
    let mut file = File::open(filename).expect("Could not find input file");
    let mut string = String::new();
    file.read_to_string(&mut string).expect("Could not read file");

    return string.lines()
        .map(|x| x.to_string())
        .collect();
}

#[derive(Debug)]
pub struct CountMap<T: Eq + Hash> {
    pub counts: HashMap<T, usize>
}

impl<T: Eq + Hash> CountMap<T> {
    pub fn new() -> Self {
        CountMap {
            counts: HashMap::new()
        }
    }

    pub fn add(&mut self, val: T, num: usize) {
        self.counts.entry(val).and_modify(|existing| {
            *existing += num
        }).or_insert(num);
    }

    pub fn increment(&mut self, val: T) {
        self.add(val, 1);
    }

    pub fn greatest(&self) -> impl Iterator<Item=(&T, usize)> {
        let max = self.counts.values().max().cloned();
        return self.counts.iter()
            .filter(move |(k, v)| Some(**v) == max)
            .map(|(k, v)| (k, *v));
    }

    pub fn least(&self) -> impl Iterator<Item=(&T, usize)> {
        let max = self.counts.values().min().cloned();
        return self.counts.iter()
            .filter(move |(k, v)| Some(**v) == max)
            .map(|(k, v)| (k, *v));
    }
}

impl<T: Eq + Hash + PartialOrd> CountMap<T> {
}
