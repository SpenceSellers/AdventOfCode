using System.Collections.Generic;

namespace AdventOfCode.AdventLib.Grid
{
    public static class EnumerableGridExtensions
    {
        public static SparseGrid<T> ToGrid<T>(this IEnumerable<(GridPoint, T)> pointValues)
        {
            var grid = new SparseGrid<T>();
            foreach (var (point, value) in pointValues)
            {
                grid.Set(point, value);
            }

            return grid;
        }

        public static SparseGrid<bool> ToGrid(this IEnumerable<GridPoint> points)
        {
            return points.SelectAlongside(_ => true).ToGrid();
        }
    }
}