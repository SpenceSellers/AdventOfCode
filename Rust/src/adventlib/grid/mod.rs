#[allow(dead_code)]
pub mod point;
pub mod solid_grid;
pub mod calculated_grid;
pub mod window;
pub use self::point::*;

pub trait GridView {
    type Item;
    fn get_cell(&self, pos: Point) -> Self::Item;

    fn windowed(self, bound: RectangleBounds) -> window::GridWindow<Self> where
        Self: std::marker::Sized
    {
        window::GridWindow {
            grid: self,
            region: bound
        }
    }
}

pub trait DefinedSizeGrid: GridView {
    fn width(&self) -> usize;
    fn height(&self) -> usize;

    fn bounds(&self) -> RectangleBounds {
        RectangleBounds::new(Point::new(0,0), Point::new(self.width() as i64, self.height() as i64)).unwrap()
    }

    fn to_solid(self) -> solid_grid::SolidGrid<<Self as GridView>::Item> where
    Self: std::marker::Sized {
        solid_grid::SolidGrid::new_from_fn(self.width(), self.height(), |x,y| self.get_cell(Point::new(x as i64,y as i64)))
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct RectangleBounds(Point, Point);

impl RectangleBounds {
    pub fn new(lesser: Point, greater: Point) -> Option<RectangleBounds> {
        if lesser.x <= greater.x && lesser.y <= greater.y {
            Some(RectangleBounds(lesser, greater))
        } else {
            None
        }
    }

    pub fn lesser(&self) -> Point { self.0 }

    pub fn greater(&self) -> Point { self.1 }

    pub fn width(&self) -> i64 { self.greater().x - self.lesser().x }

    pub fn height(&self) -> i64 { self.greater().y - self.lesser().y }

    pub fn contains(&self, point: Point) -> bool {
        point.x >= self.lesser().x && point.x < self.greater().x && point.y >= self.lesser().y && point.y < self.greater().y
    }
}

// struct OpenGrid<T> {
//     cells: HashMap<Point, T>
// }

// impl<T> Grid<Option<&T>> for OpenGrid<T> {
//     fn get(&self, pos: Point) -> Option<T> {
//         self.cells.get(&pos)
//     }
// }