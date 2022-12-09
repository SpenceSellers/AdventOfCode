using System;

namespace AdventOfCode.AdventLib.Grid
{
    public class MappedGrid<TOld, TNew>: IGrid<TNew>
    {
        private readonly IGrid<TOld> _grid;
        private readonly Func<GridPoint, TOld, TNew> _func;

        public MappedGrid(IGrid<TOld> grid, Func<GridPoint, TOld, TNew> func)
        {
            _grid = grid;
            _func = func;
        }

        public TNew Get(GridPoint point) => _func(point, _grid.Get(point));
    }
}