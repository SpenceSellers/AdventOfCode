namespace AdventOfCode.AdventLib.Grid
{
    public class GridPoint
    {
        public int X { get; }
        public int Y { get; }

        public GridPoint(int x, int y)
        {
            X = x;
            Y = y;
        }
        
        public static GridPoint Origin => new GridPoint(0, 0);
        public GridPoint Add(GridPoint other) => new GridPoint(X + other.X, Y + other.Y);
        public static GridPoint operator +(GridPoint a, GridPoint b) => a.Add(b);

        public GridPoint Scale(int scale) => new GridPoint(X * scale, Y * scale);

        public static GridPoint operator -(GridPoint a, GridPoint b) => a.Add(b.Scale(-1));

        public override string ToString()
        {
            return $"({X},{Y})";
        }
    }
}