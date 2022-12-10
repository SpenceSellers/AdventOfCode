using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib.Grid
{
    /// <summary>
    /// A point in 2D integer space. Negative coordinates are fine.
    /// </summary>
    public readonly record struct GridPoint(int X, int Y)
    // I benchmarked "struct" to cause looped grid accesses to be about 30% faster on 2022-11-15
    // It makes 2022 day 9 almost twice as fast
    {
        public static GridPoint Origin => new(0, 0);
        public GridPoint Add(GridPoint other) => new(X + other.X, Y + other.Y);
        public static GridPoint operator +(GridPoint a, GridPoint b) => a.Add(b);

        public GridPoint Scale(int scale) => new(X * scale, Y * scale);

        public static GridPoint operator *(GridPoint a, int b) => a.Scale(b);

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

        public IEnumerable<GridPoint> Adjacent4
        {
            get
            {
                yield return this + new GridPoint(-1, 0);
                yield return this + new GridPoint(+1, 0);
                yield return this + new GridPoint(0, -1);
                yield return this + new GridPoint(0, 1);
            }
        }

        public IEnumerable<GridPoint> Adjacent8
        {
            get
            {
                yield return this + new GridPoint(-1, 0);
                yield return this + new GridPoint(-1, -1);
                yield return this + new GridPoint(-1, +1);
                yield return this + new GridPoint(+1, 0);
                yield return this + new GridPoint(+1, -1);
                yield return this + new GridPoint(+1, 1);
                yield return this + new GridPoint(0, -1);
                yield return this + new GridPoint(0, 1);
            }
        }

        /// <summary>
        /// Converts (20, -7) into (1, -1).
        /// Beware of how it will change the effective angle of the point from the origin if used on non-45-degree
        /// aligned vectors.
        /// </summary>
        public GridPoint UnitAxes
        {
            get
            {
                var x = X switch
                {
                    > 0 => 1,
                    < 0 => -1,
                    0 => 0
                };
                var y = Y switch
                {
                    > 0 => 1,
                    < 0 => -1,
                    0 => 0
                };
                return new GridPoint(x, y);
            }
        }

        public IEnumerable<GridPoint> PointsBetween(GridPoint start, GridPoint end, bool inclusiveStart = true, bool inclusiveEnd = true)
        {
            var increment = (end - start).UnitAxes;
            var current = start;
            if (inclusiveStart)
            {
                yield return start;
            }
            while (current != end)
            {
                current += increment;
                if (inclusiveEnd || current != end)
                {
                    yield return current;
                }
            }
        }
    }
}