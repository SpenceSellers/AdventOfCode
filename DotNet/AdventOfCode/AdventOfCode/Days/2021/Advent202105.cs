using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2021
{
    public class Advent202105 : Problem
    {
        public override object PartOne(string[] input)
        {
            var lineSegments = input.Select(line =>
            {
                line.Split("->").Deconstruct(out var left, out var right);
                left.Split(",").Select(int.Parse).Deconstruct(out var lx, out var ly);
                right.Split(",").Select(int.Parse).Deconstruct(out var rx, out var ry);
                return (new GridPoint(lx, ly), new GridPoint(rx, ry));
            }).ToList();

            var axisAlignedSegments = lineSegments
                .Where(x => IsAxisAligned(x.Item1, x.Item2))
                .ToList();

            var grid = new SparseGrid<int>();

            var allPoints = axisAlignedSegments.SelectMany(x => PointsOnLine(x.Item1, x.Item2));

            foreach (var gridPoint in allPoints)
            {
                grid.Update(gridPoint, 0, count => count + 1);
            }

            return grid.AllDefinedCells.Values.Count(x => x != 1);
        }

        public bool IsAxisAligned(GridPoint a, GridPoint b)
        {
            return a.X == b.X || a.Y == b.Y;
        }

        public IEnumerable<GridPoint> PointsOnLine(GridPoint a, GridPoint b)
        {
            var increment = (b - a).UnitAxes;
            var last = a;
            yield return a;
            while (last != b)
            {
                last += increment;
                yield return last;
            }
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}