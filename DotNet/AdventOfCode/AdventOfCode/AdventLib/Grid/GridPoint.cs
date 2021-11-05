using System;

namespace AdventOfCode.AdventLib.Grid
{
    /// <summary>
    /// A point in 2D integer space. Negative coordinates are fine.
    /// </summary>
    public record GridPoint(int X, int Y)
    {
        public static GridPoint Origin => new(0, 0);
        public GridPoint Add(GridPoint other) => new(X + other.X, Y + other.Y);
        public static GridPoint operator +(GridPoint a, GridPoint b) => a.Add(b);

        public GridPoint Scale(int scale) => new(X * scale, Y * scale);

        public static GridPoint operator -(GridPoint a, GridPoint b) => a.Add(b.Scale(-1));

        public override string ToString() => $"({X},{Y})";

        public int ManhattanDistanceFromOrigin() => Math.Abs(X) + Math.Abs(Y);

        public GridPoint RotateAroundOrigin(int times) =>
            times switch
            {
                0 => this,
                1 => new GridPoint(Y, -X),
                -1 => new GridPoint(-Y, X),
                > 0 => RotateAroundOrigin(1).RotateAroundOrigin(times - 1),
                _ => RotateAroundOrigin(-1).RotateAroundOrigin(times + 1)
            };
    }
}