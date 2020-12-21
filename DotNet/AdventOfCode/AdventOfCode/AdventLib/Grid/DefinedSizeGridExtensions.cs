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
                // Fun, halfway useful character mappings for debugging.
                string {Length: 1} s => s[0],
                bool i => i ? 'X' : '.',
                int i when i is >= 0 and <= 9 => i.ToString()[0],
                int i when i > 9 => '>',
                int i when i < 0 => '-',
                null => ' ',
                _ => '?'
            };
        }
        
        public static IDefinedSizeGrid<T> Trace<T>(this IDefinedSizeGrid<T> grid, string label = null)
        {
            if (label != null)
            {
                Console.Out.WriteLine($"Trace {label}");
            }
            Console.Out.WriteLine(grid.Export());
            return grid;
        }

        public static IEnumerable<T> AllCells<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Region().AllPoints().Select(grid.Get);
        }

        public static IDefinedSizeGrid<T> RotateClockwise<T>(this IDefinedSizeGrid<T> grid)
        {
            Console.Out.WriteLine("Rotating");
            return grid.Warp(gp =>
                {
                    Console.Out.WriteLine("Yes");
                    var (x, y) = gp;
                    return new GridPoint(y, -x + grid.Height - 1);
                })
                .Windowed(new GridRegion(GridPoint.Origin, grid.Height, grid.Width));
        }
    }
}