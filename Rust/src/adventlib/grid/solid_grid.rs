use super::*;

pub struct SolidGrid<T> {
    rows: Vec<Vec<T>>,
    size_x: usize,
    size_y: usize
}

impl<T> SolidGrid<T> {
    pub fn new_from_fn(size_x: usize, size_y: usize, builder: impl Fn(usize, usize) -> T) -> Self {
        let mut rows = Vec::new();
        for y in 0..size_y {
            let mut row = Vec::new();
            for x in 0..size_x {
                row.push(builder(x, y));
            }
            rows.push(row);
        }

        SolidGrid { rows, size_x, size_y }
    }

    pub fn get(&self, p: Point) -> &T {
        assert!(self.bounds().contains(p));
        &self.rows[p.y as usize][p.x as usize]
    }

    // Likely move this to trait or From<>
    // pub fn view(&self) -> SolidGridView<T> {
    //     SolidGridView { grid: self }
    // }
}

impl<T: Clone> SolidGrid<T> {
    pub fn new_cloned(size_x: usize, size_y: usize, template: &T) -> Self {
        SolidGrid::new_from_fn(size_x, size_y, |_, _| template.clone())
    }
}

// pub struct SolidGridView<'a, T> {
//     grid: &'a SolidGrid<T>
// }

// impl<'a, T> GridView for SolidGridView<'a, T> {
//     type Item = Option<&'a T>;
//     fn get_cell(&self, pos: Point) -> Self::Item {
//         if self.grid.contains_point(pos) {
//             Some(self.grid.get(pos))
//         } else {
//             None
//         }
//     }
// }

impl<'a, T> GridView for &'a SolidGrid<T> {
    type Item = Option<&'a T>;
    fn get_cell(&self, pos: Point) -> Self::Item {
        if self.bounds().contains(pos) {
            Some(self.get(pos))
        } else {
            None
        }
    }
}

impl<T> DefinedSizeGrid for SolidGrid<T> {
    fn width(&self) -> usize { self.size_x }
    fn height(&self) -> usize { self.size_y }
}