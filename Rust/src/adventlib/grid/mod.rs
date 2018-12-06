use std::collections::{HashMap};

pub mod point;
pub use self::point::*;

trait Grid<T> {
    fn get(&self, pos: Point) -> T;
}

// struct OpenGrid<T> {
//     cells: HashMap<Point, T>
// }

// impl<T> Grid<Option<&T>> for OpenGrid<T> {
//     fn get(&self, pos: Point) -> Option<T> {
//         self.cells.get(&pos)
//     }
// }