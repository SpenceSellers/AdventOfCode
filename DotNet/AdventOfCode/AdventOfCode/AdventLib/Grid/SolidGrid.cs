using System.Linq;

namespace AdventOfCode.AdventLib.Grid
{
    public class SolidGrid<T> : IGrid<T>, IMutableGrid<T>
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
        
        public T Get(GridPoint point)
        {
            return _grid[point.Y][point.X];
        }

        public void Set(GridPoint point, T value)
        {
            _grid[point.Y][point.X] = value;
        }
    }
}