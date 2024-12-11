import advent
from grid.base import Grid, GridDoesNotContainPointError
from grid.point import DIRECTIONS_8, NORTHEAST, NORTHWEST, SOUTHEAST, SOUTHWEST, Point
from grid.solid import SolidGrid

directions = []


@advent.part_1
def part_1(lines: list[str]):
    g = SolidGrid.from_2d_list(lines)
    count = 0
    for point in g.region.all_points():
        for direction in DIRECTIONS_8:
            try:
                s = "".join(g.grab_from_direction(point, direction, 4))
                if s == "XMAS":
                    count += 1
            except GridDoesNotContainPointError:
                pass
    return count


def contains_x_mas(g: Grid[str], point: Point) -> bool:
    start_1 = point + NORTHWEST
    start_2 = point + SOUTHWEST
    try:
        if "".join(g.grab_from_direction(start_1, SOUTHEAST, 3)) not in ("MAS", "SAM"):
            return False

        if "".join(g.grab_from_direction(start_2, NORTHEAST, 3)) not in ("MAS", "SAM"):
            return False
    except GridDoesNotContainPointError:
        return False

    return True


@advent.part_2
def part_2(lines: list[str]):
    g = SolidGrid.from_2d_list(lines)
    count = 0
    for point in g.region.all_points():
        if contains_x_mas(g, point):
            count += 1
    return count


advent.run()
