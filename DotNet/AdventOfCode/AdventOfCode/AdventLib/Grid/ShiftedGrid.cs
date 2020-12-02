namespace AdventOfCode.AdventLib.Grid
{
    public class ShiftedGrid<T>: IGrid<T>
    {
        private readonly GridPoint _origin;
        private readonly IGrid<T> _grid;

        public ShiftedGrid(GridPoint origin, IGrid<T> grid)
        {
            _origin = origin;
            _grid = grid;
        }
        
        public T Get(GridPoint point)
        {
            return _grid.Get(point - _origin);
        }
    }
}