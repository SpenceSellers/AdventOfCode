extern crate adventlib;
use adventlib::grid::*;
use adventlib::grid::calculated_grid::*;

struct FuelGrid {
    serial_number: i64
}

impl FuelGrid {
    // fn new(serial_number: i64) -> Self {
    //     FuelGrid { serial_number }
    // }

    fn get_fuel(&self, p: Point) -> i64 {
        let cg = CalculatedGrid::new(|point| {
            let rack_id = point.x + 10;
            let mut power_level = rack_id * point.y;
            power_level += self.serial_number;
            power_level *= rack_id;
            return hundreds_digit(power_level) - 5;
        });
        cg.get_cell(p)
    }

    fn fuel_square(&self, corner: Point, size: u64) -> i64 {
        Point::in_square(corner.x..corner.x + size as i64, corner.y..corner.y + size as i64)
            .map(|p| self.get_fuel(p))
            .sum()
    }
}

fn hundreds_digit(x: i64) -> i64 {
    (x / 100) % 10
}

fn p1(grid: &FuelGrid) {
    let biggest = Point::in_square(0..300, 0..300).map(|corner| {
        let score = grid.fuel_square(corner, 3);
        (corner, score)
    })
    .max_by_key(|(_corner, score)| *score);

    println!("{:?}", biggest);
}

fn p2(grid: &FuelGrid) {
    let biggest = (1..300).into_iter().flat_map(|size| {
        println!("Trying size {}", size);
        let search_corner_bound: i64 = 300 - size;
        Point::in_square(0..search_corner_bound, 0..search_corner_bound).map(move |corner| {
            let score = grid.fuel_square(corner, size as u64);
            (corner, size, score)
        })
    }).max_by_key(|(_corner, _size, score)| *score);

    println!("{:?}", biggest);
}

fn main() {
    let serial = 1308;
    let grid = FuelGrid {
        serial_number: serial
    };
    p1(&grid);
    p2(&grid);
}

mod test {
    use super::*;
    #[test]
    fn test_hundreds_digit() {
        assert_eq!(hundreds_digit(12345), 3);
        assert_eq!(hundreds_digit(900), 9);
        assert_eq!(hundreds_digit(50), 0);
        assert_eq!(hundreds_digit(0), 0);
    }

    #[test]
    fn test_fuel() {
        let grid = FuelGrid { serial_number: 8 };
        assert_eq!(grid.get_fuel(Point::new(3,5)), 4);
    }
}