from grid.base import ComputedGrid, Grid


coordinate_grid = ComputedGrid(lambda p: p)


def constant_grid[T](v: T) -> Grid[T]:
    return ComputedGrid(lambda _: v)