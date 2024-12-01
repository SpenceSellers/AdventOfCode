from grid.base import ComputedGrid
from grid.common_grids import constant_grid, coordinate_grid
from grid.point import Point


def test_computed_grid():
    g = ComputedGrid(lambda p: p)
    assert g[Point(128, -138)] == Point(128, -138)


def test_coordinate_grid():
    assert coordinate_grid[Point(128, -138)] == Point(128, -138)


def test_constant_grid():
    g = constant_grid("wow")
    assert g[Point(128, -138)] == "wow"
