use super::*;

pub struct CalculatedGrid<F> {
    func: F
}

impl<F, T> CalculatedGrid<F> 
    where F: Fn(Point) -> T {

    pub fn new(f: F) -> Self {
        CalculatedGrid { func: f }
    }
}

impl<F, T> GridView for CalculatedGrid<F>
    where F: Fn(Point) -> T {
    type Item = T;
    
    fn get_cell(&self, pos: Point) -> T {
        unimplemented!()
    }
}