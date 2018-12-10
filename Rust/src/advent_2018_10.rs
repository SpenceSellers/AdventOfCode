extern crate adventlib;
use adventlib::grid::*;

extern crate regex;
use regex::Regex;

#[macro_use] extern crate lazy_static;

struct Particle {
    initial: Point,
    velocity: Point
}

impl Particle {
    fn pos_at(&self, tick: i64) -> Point {
        let offset = self.velocity.multiply(tick);
        return self.initial.add_point(offset);
    }
}

fn parse_particle(s: &str) -> Particle {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"position=<(.*?),(.*?)> velocity=<(.*?),(.*?)>").unwrap();
    }

    let captures = REGEX.captures(s).expect("Cannot parse line");
    let captured = |i: usize| captures
        .get(i)
        .expect("Capture not found")
        .as_str()
        .trim()
        .parse::<i64>()
        .expect("Could not parse int");

    return Particle {
        initial: Point::new(captured(1), captured(2)),
        velocity: Point::new(captured(3), captured(4))
    };
}

fn bounds(points: &[Point]) -> (Point, Point) {
    let min_x = points.iter().map(|p| p.x).min().unwrap();
    let max_x = points.iter().map(|p| p.x).max().unwrap();
    let min_y = points.iter().map(|p| p.y).min().unwrap();
    let max_y = points.iter().map(|p| p.y).max().unwrap();

    return (Point::new(min_x, min_y), Point::new(max_x, max_y))
}

fn show_points(points: &[Point]) -> String {
    let mut s = String::new();
    let (lesser_corner, greater_corner) = bounds(points);
    for y in lesser_corner.y..=greater_corner.y {
        for x in lesser_corner.x..=greater_corner.x {
           s.push(if points.contains(&Point::new(x, y)) { 'X' } else { '.' });
        }
        s.push('\n');
    }
    return s;
}

fn main() {
    let particles: Vec<_> = adventlib::read_input_lines("input.txt").iter().map(|s| parse_particle(s)).collect();
    let mut last_group_size = None;
    let mut last_points: Option<Vec<Point>> = None;
    for i in 0.. {
        let points: Vec<Point> = particles.iter().map(|p| p.pos_at(i)).collect();
        let (lesser_corner, greater_corner) = bounds(&points);
        
        let group_size = lesser_corner.manhattan_distance(greater_corner);
        if let Some(last_group_size) = last_group_size {
            if group_size > last_group_size {
                // Our group size just got bigger. Chances are the previous second was the answer.
                println!("{}:\n{}", i, show_points(&last_points.unwrap()));
                break;
            }
        }

        last_group_size = Some(group_size);
        last_points = Some(points);
    }
}