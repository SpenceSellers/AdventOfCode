using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.Grid
{
    public class SolidGrid<T> : IMutableGrid<T>, IDefinedSizeGrid<T>
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

        public SolidGrid(IDefinedSizeGrid<T> grid)
        {
            Width = grid.Width;
            Height = grid.Height;
            _grid = new T[grid.Height][];
            for (var y = 0; y < grid.Height; y++)
            {
                var row = new T[grid.Width];
                for (var x = 0; x < grid.Width; x++)
                {
                    var pos = new GridPoint(x, y);
                    row[x] = grid.Get(pos);
                }

                _grid[y] = row;
            }
        }

        public static SolidGrid<char> Extract(IEnumerable<string> lines)
        {
            return new SolidGrid<char>(lines.Select(l => l.ToCharArray()));
        }

        // We could do this though this.Region().ContainsPoint(), but this way benchmarks as quite a bit faster,
        // by about 400ms on 2020 day 11.
        private bool ContainsPoint(GridPoint p) => p.X >= 0 && p.X < Width && p.Y >= 0 && p.Y < Height;
        
        public T Get(GridPoint point)
        {

            if (!ContainsPoint(point))
            {
                throw new NonexistentCellException(point);
            }
            
            return _grid[point.Y][point.X];
        }

        public void Set(GridPoint point, T value)
        {
            _grid[point.Y][point.X] = value;
        }
    }
}