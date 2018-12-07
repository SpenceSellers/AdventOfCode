extern crate adventlib;
use adventlib::grid::*;
use adventlib::CountMap;
use std::collections::HashSet;

fn parse_point(s: &str) -> Point {
    let mut pieces = s.split(", ");
    let x = pieces.next().unwrap().parse().unwrap();
    let y = pieces.next().unwrap().parse().unwrap();

    Point::new(x, y)
}

fn closest_points(p: Point, points: &[Point]) -> Vec<(Point, u64)> {
    let mut pds: Vec<(Point, u64)> = points.iter().map(|point| (*point, p.manhattan_distance(*point))).collect();
    pds.sort_by_key(|(_, dist)| *dist);

    let mut results = Vec::new();
    let mut last_dist = None;
    for (point, dist) in pds {
        if let Some(last) = last_dist {
            if dist != last { break; }
        }
        last_dist = Some(dist);
        results.push((point, dist));
    }

    return results;
}

fn infinite_area_points(points: &[Point]) -> HashSet<Point> {
    let mut infinite = HashSet::new();

    // We're going to build a bounding wall at a large distance from the points.
    // Anything that's made it out this far will go out forever.
    let distance = 10000;
    for v in -distance..=distance {
        let top = Point::new(v, distance);
        let bottom = Point::new(v, -distance);
        let left = Point::new(-distance, v);
        let right = Point::new(distance, v);

        for point in [top, bottom, left, right].iter() {
            let closest = closest_points(*point, &points);
            for close in closest {
                infinite.insert(close.0);
            }
        }
    }

    return infinite;
}

fn p1(points: &[Point]) {
    let mut space_counts = CountMap::new();

    for x in -100..=500 {
        for y in -100..=500 {
            let point = Point::new(x, y);
            let closest = closest_points(point, &points);
            if closest.len() > 1 { continue; }

            let closest_single = closest[0].0;
            space_counts.increment(closest_single);
        }
    }

    let infinite = infinite_area_points(points);

    for (point, count) in space_counts.counts.iter() {
        if infinite.contains(point) {
            continue;
        }

        println!("Look at this and choose the biggest: {:?}: {}", point, count);
        println!("(Proprietary neural-network powered min() function)");
    }
}

fn distance_to_all(point: Point, points: &[Point]) -> u64 {
    points.iter().map(|p| point.manhattan_distance(*p)).sum()
}

fn p2(points: &[Point]) {
    let threshold = 10000;
    let mut count = 0;

    // Advent of code creator would cry at how little math I chose to learn for this problem.
    for x in -500..=500 {
        for y in -500..=500 {
            if distance_to_all(Point::new(x, y), points) < threshold {
                count += 1;
            }
        }
    }
    println!("Count: {}", count);
}


fn main() {
    let points: Vec<Point> = adventlib::read_input_lines("input.txt").iter()
        .map(|line| parse_point(line))
        .collect();

    p1(&points);
    p2(&points);
}