using System;

namespace AdventOfCode.AdventLib.Grid
{
    public class NonexistentCellException : Exception
    {
        public readonly GridPoint Point;

        public NonexistentCellException(GridPoint point) : base($"Point {point} does not exist")
        {
            Point = point;
        }
    }
}