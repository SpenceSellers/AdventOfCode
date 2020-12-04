using System;

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
    }
}