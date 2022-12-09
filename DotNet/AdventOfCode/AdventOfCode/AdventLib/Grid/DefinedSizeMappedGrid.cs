using System;

namespace AdventOfCode.AdventLib.Grid
{
    public class DefinedSizeMappedGrid<TOld, TNew>: IDefinedSizeGrid<TNew>
    {
        private readonly IDefinedSizeGrid<TOld> _baseGrid;
        private readonly Func<GridPoint, TOld, TNew> _func;
        public int Width { get; }
        public int Height { get; }

        public DefinedSizeMappedGrid(IDefinedSizeGrid<TOld> baseGrid, Func<GridPoint, TOld, TNew> func)
        {
            _baseGrid = baseGrid;
            _func = func;
            Width = baseGrid.Width;
            Height = baseGrid.Height;
        }
        public TNew Get(GridPoint point)
        {
            return _func(point, _baseGrid.Get(point));
        }
    }
}