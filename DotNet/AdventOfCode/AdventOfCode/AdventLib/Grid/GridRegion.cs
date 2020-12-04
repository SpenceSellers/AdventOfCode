using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.Grid
{
    public class GridRegion
    {
        public GridPoint Origin { get; }
        public int Width { get; }
        public int Height { get; }

        public GridRegion(GridPoint origin, int width, int height)
        {
            Origin = origin;
            Width = width;
            Height = height;
        }

        public bool ContainsPoint(GridPoint p) => p.X >= Origin.X && p.X < Origin.X + Width && p.Y >= Origin.Y && p.Y < Origin.Y + Height;

        public IEnumerable<GridPoint> AllPoints()
        {
            return Enumerable.Range(0, Height)
                .SelectMany(y => Enumerable.Range(0, Width).Select(x => new GridPoint(x, y)));
        }
    }
}