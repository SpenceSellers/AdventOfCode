use std::collections::{HashSet, HashMap};
extern crate adventlib;
use adventlib::grid::point::*;

#[derive(Debug)]
struct TrackPoint {
    kind: char,
    point: Point,
    north: bool,
    south: bool,
    east: bool,
    west: bool,
}

impl TrackPoint {
    fn new(kind: char, point: Point) -> TrackPoint {
        TrackPoint {
            kind,
            point,
            north: false,
            south: false,
            east: false,
            west: false
        }
    }

    fn directions(&self) -> Vec<Direction> {
        let mut dirs = Vec::new();
        if self.north { dirs.push(Direction::North); }
        if self.south { dirs.push(Direction::South); }
        if self.east { dirs.push(Direction::East); }
        if self.west { dirs.push(Direction::West); }
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

/// Every point that seems worth considering as a piece of track.
fn every_tracked_point(lines: &[String]) -> HashSet<(Point, char)> {
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
    let tracked_points = every_tracked_point(lines);

    // Prepare track data
    for (point, c) in tracked_points.iter() {
        track_map.insert(*point, TrackPoint::new(*c, *point));
    }

    for (point, c) in tracked_points.iter().cloned() {
        let north_point = point.shift_direction(Direction::North, 1);
        let south_point = point.shift_direction(Direction::South, 1);
        let north_could_connect = track_map.get(&north_point).map(|tp| can_connect_north_south(tp.kind)).unwrap_or(false);
        let south_could_connect = track_map.get(&south_point).map(|tp| can_connect_north_south(tp.kind)).unwrap_or(false);

        let (north, south, east, west) = match c {
            '-' => (false, false, true, true),
            '|' => (true, true, false, false),
            '/' => match (north_could_connect, south_could_connect) {
                (true, false) => (true, false, false, true),
                (false, true) => (false, true, true, false),
                _ => panic!("Ambiguous corner at {:?}!", point),
            },
            '\\' => match (north_could_connect, south_could_connect) {
                (true, false) => (true, false, true, false), 
                (false, true) => (false, true, false, true), 
                _ => panic!("Ambiguous corner at {:?}!", point),
            },
            '+' => (true, true, true, true),
            _ => {
                cart_id += 1;
                let (dir, connects) = match c {
                    'v' => (Direction::South, (true, true, false, false)),
                    '>' => (Direction::East, (false, false, true, true)),
                    '<' => (Direction::West, (false, false, true, true)),
                    '^' => (Direction::North, (true, true, false, false)),
                    _ => panic!("Unknown map symbol: {}", c)
                };
                carts.push(Cart {pos: point, id: cart_id, turns: 0, direction: dir} );
                connects
            }
        };

        let track_point = track_map.get_mut(&point).unwrap();
        track_point.north = north;
        track_point.south = south;
        track_point.east = east;
        track_point.west = west;
    }

    return (track_map, carts);
}

fn tick_cart(cart: &mut Cart, track: &HashMap<Point, TrackPoint>) {
    let next_point = cart.pos.shift_direction(cart.direction, 1);
    let next_track = track.get(&next_point).unwrap();

    let next_available_directions = next_track.directions();
    let next_direction = match next_available_directions.len() {
        // Straight track
        2 => next_available_directions.iter()
            .cloned()
            .filter(|dir| next_track.point.shift_direction(*dir, 1) != cart.pos)
            .next()
            .unwrap(),
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

fn collisions(carts: &[Cart]) -> HashSet<u64> {
    let mut collided = HashSet::new();
    for cart_a in carts.iter() {
        for cart_b in carts.iter() {
            if cart_a.id == cart_b.id { continue; }
            if cart_a.pos == cart_b.pos {
                collided.insert(cart_a.id);
                collided.insert(cart_b.id);
            }
        }
    }
    return collided;
}

fn main() {
    let input = adventlib::read_input_lines("input.txt");

    let (track, mut carts) = build_track(&input);
    while carts.len() > 1 {
        // Sort the carts so collisions happen in the correct order
        carts.sort_unstable_by(|a, b| {
            a.pos.y.cmp(&b.pos.y).then(a.pos.x.cmp(&b.pos.x))
        });

        let cart_ids: Vec<u64> = carts.iter().map(|cart| cart.id).collect();

        for id in cart_ids {
            // Only tick carts that still exist. Since carts can disappear in the middle of the tick,
            // it's possible that they _don't_ still exist.
            if let Some(mut cart) = carts.iter_mut().find(|cart| cart.id == id) {
                tick_cart(&mut cart, &track);
            }
            // Remove crashed carts
            let to_remove = collisions(&carts);
            carts.retain(|cart| !to_remove.contains(&cart.id));
        }
    }
    println!("Carts are gone: {:?}", carts);
}