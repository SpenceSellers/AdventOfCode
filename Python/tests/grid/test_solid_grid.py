from grid.point import Point
from grid.solid import SolidGrid


def test_solid():
    g = SolidGrid(10, 10, lambda p: p.x + p.y)
    assert g[Point(5, 6)] == 11

    g[Point(5, 6)] = 155
    assert g[Point(5, 6)] == 155
