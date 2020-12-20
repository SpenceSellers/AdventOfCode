using System;
using System.Reflection;

namespace AdventOfCode.AdventLib.Grid
{
    public record GridPoint
    {
        public int X { get; }
        public int Y { get; }

        public GridPoint(int x, int y)
        {
            X = x;
            Y = y;
        }
        
        public static GridPoint Origin => new(0, 0);
        public GridPoint Add(GridPoint other) => new(X + other.X, Y + other.Y);
        public static GridPoint operator +(GridPoint a, GridPoint b) => a.Add(b);

        public GridPoint Scale(int scale) => new GridPoint(X * scale, Y * scale);

        public static GridPoint operator -(GridPoint a, GridPoint b) => a.Add(b.Scale(-1));

        public override string ToString()
        {
            return $"({X},{Y})";
        }

        // public override int GetHashCode()
        // {
        //     return HashCode.Combine(X, Y);
        // }

        public int ManhattanDistanceFromOrigin()
        {
            return Math.Abs(X) + Math.Abs(Y);
        }

        public GridPoint RotateAroundOrigin(int times)
        {
            switch (times)
            {
                case 0:
                    return this;
                case 1:
                    return new GridPoint(Y, -X);
                case -1:
                    return new GridPoint(-Y, X);
            }

            if (times > 0)
            {
                return RotateAroundOrigin(1).RotateAroundOrigin(times - 1);
            }

            return RotateAroundOrigin(-1).RotateAroundOrigin(times + 1);
        }
    }
}