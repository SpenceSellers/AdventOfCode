use std::collections::{HashSet, HashMap};
extern crate adventlib;
use adventlib::grid::point::*;

#[derive(Debug)]
struct TrackPoint {
    north: Option<Point>,
    south: Option<Point>,
    east: Option<Point>,
    west: Option<Point>,
}

impl TrackPoint {
    fn new() -> TrackPoint {
        TrackPoint {
            north: None,
            south: None,
            east: None,
            west: None
        }
    }

    fn directions(&self) -> Vec<(Direction, Point)> {
        let mut dirs = Vec::new();
        if let Some(p) = self.north {
            dirs.push((Direction::North, p));
        }
        if let Some(p) = self.south {
            dirs.push((Direction::South, p));
        }
        if let Some(p) = self.east {
            dirs.push((Direction::East, p));
        }
        if let Some(p) = self.west {
            dirs.push((Direction::West, p));
        }
        return dirs;
    }
}

#[derive(Debug, Copy, Clone)]
struct Cart {
    pos: Point,
    direction: Direction,
    turns: u64
}

fn each_tracked_point(lines: &[String]) -> HashSet<(Point, char)> {
    let mut points = HashSet::new();
    for (y, line) in lines.iter().enumerate()  {
        for (x, c) in line.chars().enumerate() {
            let point = Point::new(x as i64, y as i64);
            if c != ' ' {
                points.insert((point, c));
            }
        }
    }

    return points;
}

fn build_track(lines: &[String]) -> (HashMap<Point, TrackPoint>, Vec<Cart>) {
    let mut track_map: HashMap<Point, TrackPoint> = HashMap::new();
    let mut carts = Vec::new();
    let tracked_points = each_tracked_point(lines);

    // Prepare track data
    for (point, c) in tracked_points.iter() {
        track_map.insert(*point, TrackPoint::new());
    }

    for (point, c) in tracked_points.iter().cloned() {
        let north = point.shift_direction(Direction::North, 1);
        let south = point.shift_direction(Direction::South, 1);
        let east = point.shift_direction(Direction::East, 1);
        let west = point.shift_direction(Direction::West, 1);
        match c {
            '-' => {
                track_map.get_mut(&east).expect("No track to east of -").west = Some(point);
                track_map.get_mut(&west).expect("No track to west of -").east = Some(point);
            }
            '|' => {
                track_map.get_mut(&north).expect("No track to north of |").south = Some(point);
                track_map.get_mut(&south).expect("No track to south of |").north = Some(point);
            }
            '/' => {
                match (track_map.contains_key(&north), track_map.contains_key(&south)) {
                    (true, false) => {
                        track_map.get_mut(&north).expect("No track to north of /").south = Some(point);
                        track_map.get_mut(&west).expect("No track to west of /").east = Some(point);
                    }
                    (false, true) => {
                        track_map.get_mut(&south).expect("No track to south of /").north = Some(point);
                        track_map.get_mut(&east).expect("No track to east of /").west = Some(point);

                    }
                    _ => panic!("Ambiguous corner!"),
                }
            }
            '\\' => {

            }
            '+' => {
                track_map.get_mut(&north).expect("No track to north of +").south = Some(point);
                track_map.get_mut(&south).expect("No track to south of +").north = Some(point);
                track_map.get_mut(&east).expect("No track to east of +").west = Some(point);
                track_map.get_mut(&west).expect("No track to west of +").east = Some(point);
            }
            
            'v' => carts.push(Cart {pos: point, turns: 0, direction: Direction::South} ),
            '>' => carts.push(Cart {pos: point, turns: 0, direction: Direction::East} ),
            '<' => carts.push(Cart {pos: point, turns: 0, direction: Direction::West} ),
            '^' => carts.push(Cart {pos: point, turns: 0, direction: Direction::North} ),
            _ => panic!("Unknown map symbol: {}", c)
        }
    }

    return (track_map, carts);
}

fn tick_cart(cart: &mut Cart, track: &HashMap<Point, TrackPoint>) {
    let next_point = cart.pos.shift_direction(cart.direction, 1);
    let next_track = track.get(&next_point).unwrap();

    let next_available_directions = next_track.directions();
    let next_direction = match next_available_directions.len() {
        // Straight track
        2 => next_available_directions.iter().filter(|(_, p)| *p != cart.pos).next().unwrap().0,
        // Intersection
        4 => {
            let next_dir = match cart.turns % 3 {
                // Left
                0 => cart.direction.rotate_ccw(),
                // Straight
                1 => cart.direction,
                // Right 
                2 => cart.direction.rotate_cw(),
                _ => unreachable!()
            };
            cart.turns += 1;
            next_dir
        }

        _ => {
            panic!("Unexpected number of available directions")
        }
    };

    cart.pos = next_point;
    cart.direction = next_direction;
}

fn main() {
    let input = adventlib::read_input_lines("fake_input.txt");

    let (track, mut carts) = build_track(&input);
    loop {
        for cart in carts.iter_mut() {
            tick_cart(cart, &track);
        }
        println!("{:?}", carts);
    }
    println!("{:#?}", track);
}