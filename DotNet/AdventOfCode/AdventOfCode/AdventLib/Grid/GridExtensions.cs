using System;

namespace AdventOfCode.AdventLib.Grid
{
    public static class GridExtensions
    {
        public static IGrid<TNew> Map<TOld, TNew>(this IGrid<TOld> grid, Func<TOld, TNew> func)
        {
            return new MappedGrid<TOld,TNew>(grid, func);
        }
        
        public static IDefinedSizeGrid<TNew> Map<TOld, TNew>(this IDefinedSizeGrid<TOld> grid, Func<TOld, TNew> func)
        {
            return new DefinedSizeMappedGrid<TOld,TNew>(grid, func);
        }

        public static WrappingGrid<T> Wrapping<T>(this IDefinedSizeGrid<T> grid)
        {
            return new WrappingGrid<T>(grid);
        }

        public static SolidGrid<T> Solidify<T>(this IDefinedSizeGrid<T> grid)
        {
            return new SolidGrid<T>(grid);
        }

        public static WindowGrid<T> Windowed<T>(this IGrid<T> grid, GridRegion window)
        {
            return new WindowGrid<T>(grid, window);
        }

        // AKA "zip"
        public static IGrid<TNew> Overlay<TOld1, TOld2, TNew>(
            IGrid<TOld1> a,
            IGrid<TOld2> b,
            Func<TOld1, TOld2, TNew> func)
        {
            // Maybe I'll want to implement this as an actual class eventually?
            return CommonGrids.CoordinateGrid.Map(point =>
            {
                var resultA = a.Get(point);
                var resultB = b.Get(point);
                return func(resultA, resultB);
            });
        }
    }
}