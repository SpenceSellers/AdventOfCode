using System;
using System.Collections.Generic;
using System.Linq;

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

        public GridRegion BoundingRegion()
        {
            var first = _cells.Keys.First();
            var minX = first.X;
            var maxX = first.X;
            var minY = first.Y;
            var maxY = first.Y;
            foreach (var gridPoint in _cells.Keys)
            {
                if (gridPoint.X < minX) minX = gridPoint.X;
                if (gridPoint.X > maxX) maxX = gridPoint.X;
                if (gridPoint.Y < minY) minY = gridPoint.Y;
                if (gridPoint.Y > maxY) maxY = gridPoint.Y;
            }

            return new GridRegion(new GridPoint(minX, minY), maxX - minX + 1, maxY - minY + 1);
        }

        public IGrid<T> FillingEmptySpacesWith(T defaultValue)
        {
            return new GeneratedGrid<T>(point => GetOrDefault(point, defaultValue));
        }

        public IDefinedSizeGrid<T> AsDefinedSizeNotPreservingCoordinates()
        {
            return FillingEmptySpacesWith(default).Windowed(BoundingRegion());
        }
    }
}