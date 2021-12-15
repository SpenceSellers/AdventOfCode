namespace AdventOfCode.AdventLib.Grid
{
    /// <summary>
    /// A grid where not every cell contains a value.
    ///
    /// It's not necessarily true that we can produce a list
    /// of every point that exists without checking them all, though,
    /// </summary>
    public interface ISparseGrid<out T> : IGrid<T>
    {
        public bool ContainsPoint(GridPoint p);
    }
}