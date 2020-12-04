namespace AdventOfCode.AdventLib.Grid
{
    public class WindowGrid<T>: IDefinedSizeGrid<T>
    {
        private readonly IGrid<T> _baseGrid;
        private readonly GridRegion _windowRegion;

        public WindowGrid(IGrid<T> baseGrid, GridRegion windowRegion)
        {
            _baseGrid = baseGrid;
            _windowRegion = windowRegion;
        }
        
        public T Get(GridPoint point)
        {
            if (!_windowRegion.ContainsPoint(point))
            {
                throw new NonexistentCellException();
            }
            
            // Test and fix for negative positions?
            var mappedPoint = new GridPoint(point.X - _windowRegion.Origin.X, point.Y - _windowRegion.Origin.Y);
            return _baseGrid.Get(mappedPoint);
        }

        public int Width => _windowRegion.Width;
        public int Height => _windowRegion.Height;
    }
}