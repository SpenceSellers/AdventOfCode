using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;
using AdventOfCode.AdventLib.Grid.SparseGrids;

namespace AdventOfCode.Days._2021
{
    public class Advent202105 : Problem
    {
        public override object PartOne(string[] input)
        {
            return Solve(input, true);
        }
        public override object PartTwo(string[] input)
        {
            return Solve(input, false);
        }

        private object Solve(string[] input, bool axisOnly)
        {
            var lineSegments = ParseInput(input);

            if (axisOnly)
            {
                lineSegments = lineSegments
                    .Where(x => IsAxisAligned(x.Item1, x.Item2));
            }

            var allPoints = lineSegments.SelectMany(x => PointsOnLine(x.Item1, x.Item2));

            var grid = new SparseGrid<int>();
            foreach (var gridPoint in allPoints)
            {
                grid.Update(gridPoint, 0, count => count + 1);
            }

            return grid.AllDefinedCells.Values.Count(x => x != 1);
        }

        private static IEnumerable<(GridPoint, GridPoint)> ParseInput(string[] input)
        {
            var lineSegments = input.Select(line =>
            {
                line.Split("->").Deconstruct(out var left, out var right);
                left.Split(",").Select(int.Parse).Deconstruct(out var lx, out var ly);
                right.Split(",").Select(int.Parse).Deconstruct(out var rx, out var ry);
                return (new GridPoint(lx, ly), new GridPoint(rx, ry));
            });
            return lineSegments;
        }

        private bool IsAxisAligned(GridPoint a, GridPoint b) => a.X == b.X || a.Y == b.Y;

        private IEnumerable<GridPoint> PointsOnLine(GridPoint start, GridPoint end)
        {
            var increment = (end - start).UnitAxes;
            var current = start;
            yield return start;
            while (current != end)
            {
                current += increment;
                yield return current;
            }
        }

    }
}