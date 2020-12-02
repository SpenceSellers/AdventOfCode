using System;

namespace AdventOfCode.AdventLib.Grid
{
    public class MappedGrid<TOld, TNew>: IGrid<TNew>
    {
        private readonly IGrid<TOld> _grid;
        private readonly Func<TOld, TNew> _func;

        public MappedGrid(IGrid<TOld> grid, Func<TOld, TNew> func)
        {
            _grid = grid;
            _func = func;
        }

        public TNew Get(GridPoint point) => _func(_grid.Get(point));
    }
}