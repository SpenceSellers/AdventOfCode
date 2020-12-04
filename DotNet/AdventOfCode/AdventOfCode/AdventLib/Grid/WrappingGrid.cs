namespace AdventOfCode.AdventLib.Grid
{
    public class WrappingGrid<T>: IGrid<T>
    {
        private readonly IDefinedSizeGrid<T> _baseGrid;

        public WrappingGrid(IDefinedSizeGrid<T> baseGrid)
        {
            _baseGrid = baseGrid;
        }

        public T Get(GridPoint point)
        {
            var mappedPoint = new GridPoint(point.X % _baseGrid.Width, point.Y % _baseGrid.Height);
            return _baseGrid.Get(mappedPoint);
        }
    }
}