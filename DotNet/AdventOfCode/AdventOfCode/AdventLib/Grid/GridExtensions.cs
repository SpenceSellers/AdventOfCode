using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.AdventLib.Grid
{
    public static class GridExtensions
    {
        public static IGrid<TNew> Map<TOld, TNew>(this IGrid<TOld> grid, Func<TOld, TNew> func)
        {
            return new MappedGrid<TOld,TNew>(grid, func);
        }

        public static WindowGrid<T> Windowed<T>(this IGrid<T> grid, GridRegion window)
        {
            return new WindowGrid<T>(grid, window);
        }

        // AKA "zip"
        public static IGrid<TNew> Overlay<TOld1, TOld2, TNew>(
            this IGrid<TOld1> a,
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

        // Maybe should be called "Reduce?"
        public static IGrid<TNew> OverlayMany<TOld, TNew>(
            this IEnumerable<IGrid<TOld>> grids,
            Func<IEnumerable<TOld>, TNew> func)
        {
            return CommonGrids.CoordinateGrid.Map(point => func(grids.Select(g => g.Get(point))));
        }

        // A little ambitious. Applies a grid of functions to a grid of values.
        public static IGrid<TNew> Apply<TOld, TNew>(this IGrid<Func<TOld, TNew>> funcs, IGrid<TOld> argumentGrid)
        {
            return funcs.Overlay(argumentGrid, (f, x) => f(x));
        }

        /// <summary>
        /// Map the _coordinates_ of a grid
        /// </summary>
        public static IGrid<T> Warp<T>(this IGrid<T> grid, Func<GridPoint, GridPoint> warp)
        {
            return new GeneratedGrid<T>(p =>
            {
                var mappedPoint = warp(p);
                return grid.Get(mappedPoint);
            });
        }
    }
}