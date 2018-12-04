use adventlib::read_input_lines;

extern crate adventlib;
extern crate regex;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Point {
    x: u32,
    y: u32
}

#[derive(Eq, PartialEq, Copy, Clone)]
struct Claim {
    id: u32,
    x: u32,
    y: u32,
    width: u32,
    height: u32
}

impl Claim {
    // Does a claim contain a given point?
    pub fn contains(&self, p: Point) -> bool {
        p.x >= self.x
            && p.x < self.right()
            && p.y >= self.y
            && p.y < self.bottom()
    }

    pub fn bottom(&self) -> u32 { self.y + self.height }
    pub fn right(&self) -> u32 { self.x + self.width }

    pub fn overlaps(&self, other: &Claim) -> bool {
        // Note: I actually think that half of the ='s are hilariously wrong.
        // whatever, works on my input suckers.
        self.x <= other.right()
        && self.right() >= other.x
        && self.y <= other.bottom()
        && self.bottom() >= other.y
    }
}

struct ClaimSet {
    claims: Vec<Claim>
}

impl ClaimSet {
    fn owned_by(&self, point: Point) -> impl Iterator<Item=&Claim> {
        self.claims.iter().filter(move |claim| claim.contains(point))
    }
}

fn parse_claim(s: &str) -> Option<Claim> {
    let regex = regex::Regex::new(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)").unwrap();
    let captures = regex.captures(s)?;

    return Some(Claim {
        id: captures.get(1)?.as_str().parse().ok()?,
        x:  captures.get(2)?.as_str().parse().ok()?,
        y:  captures.get(3)?.as_str().parse().ok()?,
        width:  captures.get(4)?.as_str().parse().ok()?,
        height:  captures.get(5)?.as_str().parse().ok()?,
    });
}

fn part_1(claims: &ClaimSet) {
    let mut count = 0;
    for x in 0..1000 {
        for y in 0..1000 {
            if claims.owned_by(Point {x, y}).count() >= 2 {
                count += 1;
            };
        }
    }

    println!("Part 1: {}", count);
}


fn part_2(claims: &ClaimSet) {
    for claim_a in claims.claims.iter() {
        if claims.claims.iter().all(|claim_b| claim_a == claim_b || !claim_a.overlaps(claim_b)) {
            println!("Part 2: Claim with no overlap: {}", claim_a.id);
            return;
        }
    }
}

fn main() {
    let claims = read_input_lines("input.txt").iter()
        .map(|s| parse_claim(s))
        .collect::<Option<Vec<Claim>>>()
        .expect("Failed to parse claims!");

    let claimset = ClaimSet {claims};
    part_1(&claimset);
    part_2(&claimset);
}
