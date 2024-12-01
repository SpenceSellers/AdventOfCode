from grid.point import Point, Region


def test_region():
    r = Region(Point(5, 10), 10, 15)
    assert r.upper_right == Point(15, 25)
    assert r.area == 150


def test_region_from_corners():
    r = Region.from_corners(Point(5, 5), Point(6, 6), inclusive=True)
    assert r.width == 2
    r = Region.from_corners(Point(6, 6), Point(5, 5), inclusive=True)
    assert r.width == 2


def test_region_contains_point():
    r = Region(Point(5, 6), 2, 1)
    assert r.contains_point(Point(5, 6))
    assert r.contains_point(Point(6, 6))
    assert not r.contains_point(Point(7, 6))
    assert not r.contains_point(Point(5, 7))
    assert not r.contains_point(Point(5, 5))

    r = Region(Point(5, 6), 0, 0)
    assert not r.contains_point(Point(5, 6))
