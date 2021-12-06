using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib.Grid
{
    public class SparseGrid<T> : IMutableGrid<T>
    {
        private readonly Dictionary<GridPoint, T> _cells = new();
        public T Get(GridPoint point)
        {
            return _cells.ContainsKey(point) ? _cells[point] : throw new NonexistentCellException(point);
        }

        public T GetOrDefault(GridPoint point, T defaultValue)
        {
            return _cells.ContainsKey(point) ? _cells[point] : defaultValue;
        }

        public void Set(GridPoint point, T value)
        {
            _cells[point] = value;
        }

        public bool ContainsPoint(GridPoint p)
        {
            return _cells.ContainsKey(p);
        }

        // Todo this should probably be a mutable+sparse grid extension method
        public void Update(GridPoint p, T initialValue, Func<T, T> updater)
        {
            Set(p, updater(GetOrDefault(p, initialValue)));
        }

        public IReadOnlyDictionary<GridPoint, T> AllDefinedCells => _cells;
    }
}