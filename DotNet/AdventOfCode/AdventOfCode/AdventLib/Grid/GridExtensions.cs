using System;

namespace AdventOfCode.AdventLib.Grid
{
    public static class GridExtensions
    {
        public static IGrid<TNew> Map<TOld, TNew>(this IGrid<TOld> grid, Func<TOld, TNew> func)
        {
            return new MappedGrid<TOld,TNew>(grid, func);
        }

        public static WrappingGrid<T> Wrapping<T>(this IDefinedSizeGrid<T> grid)
        {
            return new WrappingGrid<T>(grid);
        }
    }
}