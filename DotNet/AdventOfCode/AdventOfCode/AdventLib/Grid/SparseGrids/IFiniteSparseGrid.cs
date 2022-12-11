using System.Collections.Generic;

namespace AdventOfCode.AdventLib.Grid.SparseGrids
{
    /// <summary>
    /// A grid where not all cells are filled in, but we KNOW which cells are filled in and we can list them all
    /// </summary>
    public interface IFiniteSparseGrid<T> : ISparseGrid<T>
    {
        public IReadOnlyDictionary<GridPoint, T> AllDefinedCells { get; }
    }
}