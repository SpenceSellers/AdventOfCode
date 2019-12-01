use std::ops::Range;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct Point {
    pub x: i64,
    pub y: i64,
}

impl Point {
    pub fn new(x: i64, y: i64) -> Point {
        Point {x, y}
    }

    pub fn add_point(self, point: Point) -> Point {
        return Point {
            x: self.x + point.x,
            y: self.y + point.y
        }
    }

    pub fn subtract_point(self, point: Point) -> Point {
        return Point {
            x: self.x - point.x,
            y: self.y - point.y
        }
    }

    pub fn multiply(self, n: i64) -> Point {
        Point {x: self.x * n, y: self.y * n }
    }

    pub fn shift_direction(self, dir: Direction, n: i64) -> Point {
        let dp = match dir {
            Direction::North => Point {x: 0, y: -n},
            Direction::South => Point {x: 0, y: n},
            Direction::East => Point {x: n, y: 0},
            Direction::West => Point {x: -n, y: 0}
        };

        return self.add_point(dp);
    }

    pub fn manhattan_distance(self, other: Point) -> u64 {
       (self.x - other.x).abs() as u64 + (self.y - other.y).abs() as u64
    }

    pub fn in_square(xrange: Range<i64>, yrange: Range<i64>) -> impl Iterator<Item=Point> {
        xrange.into_iter().flat_map(
            move |x| yrange.clone().into_iter().map(move |y| Point::new(x, y))
        )
    }

    /// Points in a horizontal line, end-exclusive.
    pub fn in_x_line(y: i64, x_start: i64, x_end: i64) -> impl Iterator<Item=Point> {
        assert!(x_start <= x_end);
        (x_start..x_end).into_iter().map(move |x| Point::new(x, y))
    }

    /// Points in a vertical line, end-exclusive.
    pub fn in_y_line(x: i64, y_start: i64, y_end: i64) -> impl Iterator<Item=Point> {
        assert!(y_start <= y_end);
        (y_start..y_end).into_iter().map(move |y| Point::new(x, y))
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum Direction {
    North,
    South,
    East,
    West
}

impl Direction {
    pub fn rotate_cw(self) -> Self {
        match self {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        }
    }

    pub fn rotate_ccw(self) -> Self {
        match self {
            Direction::North => Direction::West,
            Direction::East => Direction::North,
            Direction::South => Direction::East,
            Direction::West => Direction::South,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct DirectionSet {
    pub north: bool,
    pub south: bool,
    pub east: bool,
    pub west: bool,
}

impl DirectionSet {
    pub fn directions(&self) -> Vec<Direction> {
        let mut dirs = Vec::new();
        if self.north { dirs.push(Direction::North); }
        if self.south { dirs.push(Direction::South); }
        if self.east { dirs.push(Direction::East); }
        if self.west { dirs.push(Direction::West); }
        return dirs;
    }

    pub fn from_str(s: &str) -> Option<DirectionSet> {
        let mut dset: DirectionSet = Default::default();
        for c in s.chars() {
            match c {
                'n' => { dset.north = true; }
                's' => { dset.south = true; }
                'e' => { dset.east = true; }
                'w' => { dset.west = true; }
                _ => { return None; }
            }
        }
        return Some(dset);
    }
}

mod test {
    use super::*;
    #[test]
    fn test_manhattan_distance() {
        assert_eq!(Point::new(0,0).manhattan_distance(Point::new(0,0)), 0);
        assert_eq!(Point::new(1,0).manhattan_distance(Point::new(0,0)), 1);
        assert_eq!(Point::new(0,10).manhattan_distance(Point::new(0,0)), 10);
        assert_eq!(Point::new(0,0).manhattan_distance(Point::new(0,10)), 10);
        assert_eq!(Point::new(10,10).manhattan_distance(Point::new(0,0)), 20);
        assert_eq!(Point::new(0,-10).manhattan_distance(Point::new(0,0)), 10);
    }
}