using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.Grid
{
    public record GridRegion(GridPoint Origin, int Width, int Height)
    {
        public bool ContainsPoint(GridPoint p) => p.X >= Origin.X && p.X < Origin.X + Width && p.Y >= Origin.Y && p.Y < Origin.Y + Height;

        public bool ContainsRegion(GridRegion other) =>
            other.Origin.X >= Origin.X &&
            other.Origin.Y >= Origin.Y &&
            other.Origin.X + other.Width <= Origin.X + Width &&
            other.Origin.Y + other.Height <= Origin.Y + Height;

        public IEnumerable<GridPoint> AllPoints()
        {
            // Should this start at origin, or zero?
            return Enumerable.Range(Origin.Y, Height)
                .SelectMany(y => Enumerable.Range(Origin.X, Width).Select(x => new GridPoint(x, y)));
        }

        public IEnumerable<int> XCoords() => Enumerable.Range(Origin.X, Width);
        public IEnumerable<int> YCoords() => Enumerable.Range(Origin.Y, Height);
    }
}