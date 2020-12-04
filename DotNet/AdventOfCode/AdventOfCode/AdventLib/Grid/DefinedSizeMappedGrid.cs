using System;

namespace AdventOfCode.AdventLib.Grid
{
    public class DefinedSizeMappedGrid<TOld, TNew>: IDefinedSizeGrid<TNew>
    {
        private readonly IDefinedSizeGrid<TOld> _baseGrid;
        private readonly Func<TOld, TNew> _func;
        public int Width { get; }
        public int Height { get; }

        public DefinedSizeMappedGrid(IDefinedSizeGrid<TOld> baseGrid, Func<TOld, TNew> func)
        {
            _baseGrid = baseGrid;
            _func = func;
            Width = baseGrid.Width;
            Height = baseGrid.Height;
        }
        public TNew Get(GridPoint point)
        {
            return _func(_baseGrid.Get(point));
        }
    }
}