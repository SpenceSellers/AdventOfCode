extern crate adventlib;
use adventlib::grid::*;

fn parse_point(s: &str) -> Point {
    let mut pieces = s.split(",");
    let x = pieces.next().unwrap().parse().unwrap();
    let y = pieces.next().unwrap().parse().unwrap();

    Point::new(x, y)
}

fn closest_points(p: Point, points: &[Point]) -> Point {
    // *points.iter().min_by_key(|other_point| p.manhattan_distance(**other_point)).expect("There weren't any points")
    let mut pds: Vec<(Point, u64)> = points.iter().map(|point| (*point, p.manhattan_distance(*point))).collect();
    pds.sort_by_key(|(_, dist)| dist);

    return *pds.get(0).unwrap().0;
}

fn main() {
    let points: Vec<Point> = adventlib::read_input_lines("input.txt").iter()
        .map(|line| parse_point(line))
        .collect();

    for point in points {

    }
}