namespace AdventOfCode.AdventLib.Grid.SparseGrids
{
    public static class SparseGridExtensions
    {
        public static T GetOrDefault<T>(this ISparseGrid<T> grid, GridPoint point, T defaultValue)
        {
            return grid.ContainsPoint(point) ? grid.Get(point) : defaultValue;
        }
    }
}