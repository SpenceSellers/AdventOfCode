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
            // It's important that we check against OUR region, not the underlying grid's region.
            // The whole point is that we're hiding that region.
            if (!this.Region().ContainsPoint(point))
            {
                throw new NonexistentCellException(point);
            }

            var mappedPoint = new GridPoint(point.X + _windowRegion.Origin.X, point.Y + _windowRegion.Origin.Y);
            return _baseGrid.Get(mappedPoint);
        }

        public int Width => _windowRegion.Width;
        public int Height => _windowRegion.Height;
    }
}