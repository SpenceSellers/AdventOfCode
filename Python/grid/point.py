from dataclasses import dataclass
from typing import Iterator


@dataclass(slots=True, frozen=True)
class Point:
    x: int
    y: int

    def __add__(self, other: "Point") -> "Point":
        return Point(self.x + other.x, self.y + other.y)

    def __sub__(self, other: "Point") -> "Point":
        return Point(self.x - other.x, self.y - other.y)

    def __mul__(self, other: int) -> "Point":
        return Point(self.x * other, self.y * other)

    def manhattan_distance_from_origin(self) -> int:
        return abs(self.x) + abs(self.y)

    def manhattan_distance(self, other: "Point") -> int:
        return abs(self.x - other.x) + abs(self.y - other.y)


@dataclass(slots=True)
class Region:
    lower_left: Point
    width: int
    height: int

    def __init__(self, start: Point, width: int, height: int):
        assert width >= 0
        assert height >= 0

        self.lower_left = start
        self.width = width
        self.height = height

    @property
    def upper_right(self) -> Point:
        return self.lower_left + Point(self.width, self.height)

    @property
    def area(self) -> int:
        return self.width * self.height

    def contains_point(self, p: Point) -> bool:
        if not self.lower_left.x <= p.x < self.lower_left.x + self.width:
            return False
        if not self.lower_left.y <= p.y < self.lower_left.y + self.height:
            return False
        return True

    def all_points(self) -> Iterator[Point]:
        for y in self.all_ys():
            for x in self.all_xs():
                yield Point(x, y)

    def all_xs(self) -> Iterator[int]:
        yield from range(self.lower_left.x, self.lower_left.x + self.width)

    def all_ys(self) -> Iterator[int]:
        yield from range(self.lower_left.y, self.lower_left.y + self.height)

    @staticmethod
    def from_corners(p1: Point, p2: Point, inclusive: bool = False) -> "Region":
        min_x = min(p1.x, p2.x)
        min_y = min(p1.y, p2.y)
        max_x = max(p1.x, p2.x)
        max_y = max(p1.y, p2.y)

        lower_left = Point(min_x, min_y)

        width = max_x - min_x
        height = max_y - min_y

        if inclusive:
            width = width + 1
            height = height + 1

        return Region(lower_left, width, height)


SOUTH = Point(0, 1)
NORTH = Point(0, -1)
EAST = Point(1, 0)
WEST = Point(-1, 0)
NORTHEAST = Point(1, -1)
SOUTHEAST = Point(1, 1)
NORTHWEST = Point(-1, -1)
SOUTHWEST = Point(-1, 1)

DIRECTIONS_8 = [NORTH, NORTHEAST, EAST, SOUTHEAST, SOUTH, SOUTHWEST, WEST, NORTHWEST]
