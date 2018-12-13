#![feature(drain_filter)]
use std::collections::{HashSet, HashMap};
extern crate adventlib;
use adventlib::grid::point::*;

#[derive(Debug)]
struct TrackPoint {
    kind: char,
    north: Option<Point>,
    south: Option<Point>,
    east: Option<Point>,
    west: Option<Point>,
}

impl TrackPoint {
    fn new(kind: char) -> TrackPoint {
        TrackPoint {
            kind,
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
    id: u64,
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

fn can_connect_north_south(c: char) -> bool {
    match c {
        '|' | '+' |'^' | 'v' => true,
        _ => false
    }
}

fn build_track(lines: &[String]) -> (HashMap<Point, TrackPoint>, Vec<Cart>) {
    let mut track_map: HashMap<Point, TrackPoint> = HashMap::new();
    let mut carts = Vec::new();
    let mut cart_id = 0;
    let tracked_points = each_tracked_point(lines);

    // Prepare track data
    for (point, c) in tracked_points.iter() {
        track_map.insert(*point, TrackPoint::new(*c));
    }

    for (point, c) in tracked_points.iter().cloned() {
        let north = point.shift_direction(Direction::North, 1);
        let south = point.shift_direction(Direction::South, 1);
        let east = point.shift_direction(Direction::East, 1);
        let west = point.shift_direction(Direction::West, 1);
        let north_could_connect = track_map.get(&north).map(|tp| can_connect_north_south(tp.kind)).unwrap_or(false);
        let south_could_connect = track_map.get(&south).map(|tp| can_connect_north_south(tp.kind)).unwrap_or(false);
        let track_point = track_map.get_mut(&point).unwrap();
        match c {
            '-' => {
                track_point.east = Some(east);
                track_point.west = Some(west);
            }
            '|' => {
                track_point.north = Some(north);
                track_point.south = Some(south);
            }
            '/' => {
                match (north_could_connect, south_could_connect) {
                    (true, false) => {
                        track_point.north = Some(north);
                        track_point.west = Some(west);
                    }
                    (false, true) => {
                        track_point.south = Some(south);
                        track_point.east = Some(east);
                    }
                    _ => panic!("Ambiguous corner at {:?}!", point),
                }
            }
            '\\' => {
                match (north_could_connect, south_could_connect) {
                    (true, false) => {
                        track_point.north = Some(north);
                        track_point.east = Some(east)
                    }
                    (false, true) => {
                        track_point.south = Some(south);
                        track_point.west = Some(west);
                    }
                    _ => panic!("Ambiguous corner at {:?}!", point),
                }
            }
            '+' => {
                track_point.north = Some(north);
                track_point.south = Some(south);
                track_point.east = Some(east);
                track_point.west = Some(west);
            }
            'v' => {
                carts.push(Cart {pos: point, id: cart_id, turns: 0, direction: Direction::South} );
                track_point.north = Some(north);
                track_point.south = Some(south);
                cart_id += 1;
            }
            '>' => {
                carts.push(Cart {pos: point, id: cart_id, turns: 0, direction: Direction::East} );
                track_point.east = Some(east);
                track_point.west = Some(west);
                cart_id += 1;
            }
            '<' => {
                carts.push(Cart {pos: point, id: cart_id, turns: 0, direction: Direction::West} );
                track_point.east = Some(east);
                track_point.west = Some(west);
                cart_id += 1;
            }
            '^' => {
                carts.push(Cart {pos: point, id: cart_id, turns: 0, direction: Direction::North} );
                track_point.north = Some(north);
                track_point.south = Some(south);
                cart_id += 1;
            }
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

// let (other_cart_i, _) = carts.iter().enumerate().filter(|(ci, c)| c.pos == cart.pos).next().unwrap();
fn main() {
    let input = adventlib::read_input_lines("input.txt");

    let (track, mut carts) = build_track(&input);
    loop {
        println!("full tick");
        carts.sort_unstable_by(|a, b| {
            a.pos.y.cmp(&b.pos.y).then(a.pos.x.cmp(&b.pos.x))
        });

        let cart_ids: Vec<u64> = carts.iter().map(|cart| cart.id).collect();

        for id in cart_ids {
            println!("ticking cart {}", id);
            if let Some(mut cart) = carts.iter_mut().find(|cart| cart.id == id) {
                tick_cart(&mut cart, &track);
            } else {
                println!("Cart is already gone");
            }
            let mut to_remove = HashSet::new();
            for cart_a in carts.iter() {
                for cart_b in carts.iter() {
                    if cart_a.id == cart_b.id { continue; }
                    if cart_a.pos == cart_b.pos {
                        println!("CRASH between carts {} and {}", cart_a.id, cart_b.id);
                        to_remove.insert(cart_a.id);
                        to_remove.insert(cart_b.id);
                    }
                }
            }

            carts.retain(|cart| !to_remove.contains(&cart.id));
        }

        if carts.len() <= 1 {
            println!("Carts are gone: {:?}", carts);
            break;
        }
    }
    // println!("{:#?}", track);
}