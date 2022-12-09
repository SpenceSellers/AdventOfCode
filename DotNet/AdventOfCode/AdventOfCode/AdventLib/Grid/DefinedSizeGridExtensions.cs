using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.Grid
{
    public static class DefinedSizeGridExtensions
    {
        public static T GetOrDefault<T>(this IDefinedSizeGrid<T> grid, GridPoint p, T defaultValue = default)
        {
            return grid.Region().ContainsPoint(p) ? grid.Get(p) : defaultValue;
        }

        public static IDefinedSizeGrid<TNew> Map<TOld, TNew>(this IDefinedSizeGrid<TOld> grid, Func<TOld, TNew> func)
        {
            return new DefinedSizeMappedGrid<TOld,TNew>(grid, (_, value) => func(value));
        }

        public static IDefinedSizeGrid<TNew> Map<TOld, TNew>(this IDefinedSizeGrid<TOld> grid, Func<GridPoint, TOld, TNew> func)
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

        /// <summary>
        /// Perform some operations on a grid, but keep the outline of the original.
        /// </summary>
        public static WindowGrid<TOut> KeepingRegion<TIn, TOut>(
            this IDefinedSizeGrid<TIn> grid,
            Func<IDefinedSizeGrid<TIn>, IGrid<TOut>> func)
        {
            var region = grid.Region();
            var result = func(grid);
            if (result is IDefinedSizeGrid<TOut> definedResult)
            {
                if (!definedResult.Region().ContainsRegion(region))
                {
                    throw new InvalidOperationException(
                        $"KeepingRegion() function returned a defined size grid that changes the region of the grid");
                }
            }
            return new WindowGrid<TOut>(result, region);
        }

        /// <summary>
        /// Maps points inside the inner grid to the inner grid, but everything outside comes from the outer grid.
        /// </summary>
        public static IGrid<T> EmbedInside<T>(this IDefinedSizeGrid<T> innerGrid, IGrid<T> outerGrid)
        {
            return new GeneratedGrid<T>(p => innerGrid.Region().ContainsPoint(p) ? innerGrid.Get(p) : outerGrid.Get(p));
        }

        public static string Dump<T>(this IDefinedSizeGrid<T> grid)
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
                int i and >= 0 and <= 9 => i.ToString()[0],
                > 9 => '>',
                < 0 => '-',
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
            Console.Out.WriteLine(grid.Dump());
            return grid;
        }

        public static IEnumerable<T> AllCells<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Region().AllPoints().Select(grid.Get);
        }

        public static IEnumerable<(GridPoint Point, T CellValue)> AllEntries<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Region().AllPoints().Select(p => (p, grid.Get(p)));
        }

        public static IDefinedSizeGrid<T> RotateClockwise<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Warp(gp =>
                {
                    var (x, y) = gp;
                    return new GridPoint(y, -x + grid.Height - 1);
                })
                .Windowed(new GridRegion(GridPoint.Origin, grid.Height, grid.Width));
        }

        public static IDefinedSizeGrid<T> FlipVertically<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Warp(gp =>
            {
                var (x, y) = gp;
                return new GridPoint(x, grid.Height - y - 1);
            }).Windowed(grid.Region());
        }
        
        public static IDefinedSizeGrid<T> FlipHorizontally<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Warp(gp =>
            {
                var (x, y) = gp;
                return new GridPoint(grid.Width - x - 1, y);
            }).Windowed(grid.Region());
        }

        public static IEnumerable<T> GetRow<T>(this IDefinedSizeGrid<T> grid, int y)
        {
            return grid.Region().XCoords().Select(x => grid.Get(new GridPoint(x, y)));
        }

        public static IEnumerable<T> GetColumn<T>(this IDefinedSizeGrid<T> grid, int x)
        {
            return grid.Region().YCoords().Select(y => grid.Get(new GridPoint(x, y)));
        }

        public static IEnumerable<IEnumerable<T>> Rows<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Region().YCoords().Select(grid.GetRow);
        }

        public static IEnumerable<IEnumerable<T>> Columns<T>(this IDefinedSizeGrid<T> grid)
        {
            return grid.Region().XCoords().Select(grid.GetColumn);
        }

        public static int Count<T>(this IDefinedSizeGrid<T> grid, Func<T, bool> predicate)
        {
            return grid.AllCells().Count(predicate);
        }
    }
}