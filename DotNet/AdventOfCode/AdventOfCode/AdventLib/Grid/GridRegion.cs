using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.Grid
{
    public record GridRegion(GridPoint Origin, int Width, int Height)
    {
        public bool ContainsPoint(GridPoint p) => p.X >= Origin.X && p.X < Origin.X + Width && p.Y >= Origin.Y && p.Y < Origin.Y + Height;

        public IEnumerable<GridPoint> AllPoints()
        {
            return Enumerable.Range(0, Height)
                .SelectMany(y => Enumerable.Range(0, Width).Select(x => new GridPoint(x, y)));
        }
    }
}