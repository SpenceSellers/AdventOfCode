using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.Grid
{
    public static class DefinedSizeGridExtensions
    {
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

        public static GridRegion Region<T>(this IDefinedSizeGrid<T> grid)
        {
            return new GridRegion(GridPoint.Origin, grid.Width, grid.Height);
        }

        public static string Export<T>(this IDefinedSizeGrid<T> grid)
        {
            return string.Join("\n", Enumerable.Range(0, grid.Height).Select(y =>
                string.Join("", Enumerable.Range(0, grid.Width).Select(x => CellChar(grid.Get(new GridPoint(x, y)))))));
        }
        
        private static char CellChar<T>(T item)
        {
            return item switch
            {
                char c => c,
                bool i => i ? 'X' : '.',
                null => ' ',
                _ => '?'
            };
        }

        public static IEnumerable<T> AllCells<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Region().AllPoints().Select(grid.Get);
        }

        public static IDefinedSizeGrid<T> Trace<T>(this IDefinedSizeGrid<T> grid)
        {
            Console.Out.WriteLine(grid.Export());
            return grid;
        }
    }
}