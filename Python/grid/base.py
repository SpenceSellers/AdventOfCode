from abc import ABC, abstractmethod
from typing import Callable, override

from grid.point import Point, Region


class Grid[T](ABC):
    @abstractmethod
    def __getitem__(self, p: Point) -> T:
        raise NotImplementedError()

    def __setitem__(self, p: Point, value: T) -> None:
        raise NotMutableGridError()

    def contains_point(self, p: Point) -> bool:
        return True


class MappedGrid[Old, New](Grid[New]):
    def __init__(self, grid: Grid[Old], func: Callable[[Old], New]):
        self.grid = grid
        self.func = func

    @override
    def __getitem__(self, p: Point) -> New:
        return self.func(self.grid[p])

    @override
    def contains_point(self, p: Point) -> bool:
        return self.grid.contains_point(p)


class ComputedGrid[T](Grid[T]):
    def __init__(self, func: Callable[[Point], T]) -> None:
        super().__init__()
        self.func = func

    @override
    def __getitem__(self, p: Point) -> T:
        return self.func(p)


class WindowedGrid[T](Grid[T]):
    def __init__(self, grid: Grid[T], region: Region):
        self.grid = grid
        self.region = region

    @override
    def __getitem__(self, p: Point) -> T:
        if not self.region.contains_point(p):
            raise GridDoesNotContainPointError

        return self.grid[p - self.region.lower_left]

    @override
    def __setitem__(self, p: Point, value: T):
        if not self.region.contains_point(p):
            raise GridDoesNotContainPointError
        self.grid[p - self.region.lower_left] = value

    @override
    def contains_point(self, p: Point) -> bool:
        return self.region.contains_point(p) and self.grid.contains_point(
            p - self.region.lower_left
        )


class NotMutableGridError(Exception):
    pass


class GridDoesNotContainPointError(Exception):
    pass
