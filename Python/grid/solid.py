from grid.base import Grid, GridDoesNotContainPointError
from grid.point import Point, Region


from typing import Callable, Sequence, override


class SolidGrid[T](Grid[T]):
    def __init__(self, width: int, height: int, constructor_fn: Callable[[Point], T]):
        if width <= 0:
            raise ValueError(f"Invalid grid width: {width}")
        if height <= 0:
            raise ValueError(f"Invalid grid height: {height}")
        self.width = width
        self.height = height
        self.rows: list[list[T]] = []
        for y in range(height):
            row: list[T] = []
            for x in range(width):
                row.append(constructor_fn(Point(x, y)))
            self.rows.append(row)

    @staticmethod
    def from_2d_list[TX](items: Sequence[Sequence[TX]]) -> "SolidGrid[TX]":
        height = len(items)
        if height <= 0:
            raise ValueError("Cannot construct grid from empty list")
        width = len(items[0])
        return SolidGrid[TX](width, height, lambda p: items[p.y][p.x])

    @property
    def region(self) -> Region:
        return Region(Point(0, 0), self.width, self.height)

    @override
    def __getitem__(self, p: Point) -> T:
        if not self.contains_point(p):
            raise GridDoesNotContainPointError()
        return self.rows[p.y][p.x]

    @override
    def __setitem__(self, p: Point, value: T):
        if not self.contains_point(p):
            raise GridDoesNotContainPointError()
        self.rows[p.y][p.x] = value

    @override
    def contains_point(self, p: Point) -> bool:
        return 0 <= p.y < self.height and 0 <= p.x < self.width

    @override
    def get_bounding_box(self) -> Region | None:
        return self.region
