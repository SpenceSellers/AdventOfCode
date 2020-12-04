using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;

namespace AdventOfCode.AdventLib.Grid
{
    public class SolidGrid<T> : IGrid<T>, IMutableGrid<T>
    {
        public int Width { get; }
        public int Height { get; }
        
        private readonly T[][] _grid;
        public SolidGrid(int width, int height, T initial)
        {
            Width = width;
            Height = height;
            _grid = new T[height][];
            for (var i = 0; i < height; i++)
            {
                _grid[i] = Enumerable.Repeat(initial, width).ToArray();
            }
        }

        public SolidGrid(IEnumerable<IEnumerable<T>> data)
        {
            _grid = data.Select(line => line.ToArray()).ToArray();
            Height = _grid.Length;
            Width = _grid[0].Length;

            if (_grid.Any(line => line.Length != Width))
            {
                throw new ArgumentException($"Imported grid has jagged rows! Expected {Width}.");
            }
        }

        public static SolidGrid<char> Extract(IEnumerable<string> lines)
        {
            return new SolidGrid<char>(lines.Select(l => l.ToCharArray()));
        }
        
        public T Get(GridPoint point)
        {
            return _grid[point.Y][point.X];
        }

        public void Set(GridPoint point, T value)
        {
            _grid[point.Y][point.X] = value;
        }
    }
}