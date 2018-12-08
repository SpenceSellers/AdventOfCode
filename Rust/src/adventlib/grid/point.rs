
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

    pub fn shift_direction(self, dir: Direction, n: i64) -> Point {
        let dp = match dir {
            Direction::North => Point {x: 0, y: n},
            Direction::South => Point {x: 0, y: -n},
            Direction::East => Point {x: n, y: 0},
            Direction::West => Point {x: -n, y: 0}
        };

        return self.add_point(dp);
    }

    pub fn manhattan_distance(self, other: Point) -> u64 {
       (self.x - other.x).abs() as u64 + (self.y - other.y).abs() as u64
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