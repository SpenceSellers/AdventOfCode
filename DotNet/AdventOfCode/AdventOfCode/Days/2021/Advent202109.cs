using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2021
{
    public class Advent202109 : Problem
    {
        public override object PartOne(string[] input)
        {
            var grid = input.ToGrid().Map(c => int.Parse(c.ToString()));

            return FindLowestPoints(grid)
                .Select(x => grid.Get(x) + 1)
                .Sum();
        }

        public override object PartTwo(string[] input)
        {
            var grid = input.ToGrid().Map(c => int.Parse(c.ToString()));

            // Basins begin at the lowest points
            return FindLowestPoints(grid)
                // ... and expand outwards from there
                .Select(lowest => ExpandBasin(grid, lowest))
                // Multiply together the size of the biggest 3 basins
                .Select(basin => basin.Count)
                .OrderByDescending(size => size)
                .Take(3)
                .Product();
        }

        private static IEnumerable<GridPoint> FindLowestPoints(IDefinedSizeGrid<int> grid) =>
            grid
                .Region()
                .AllPoints()
                .Where(point =>
                {
                    var height = grid.Get(point);
                    // A point is the lowest point if all points around it are higher.
                    return point
                        .Adjacent4
                        .All(adjacentPoint => grid.GetOrDefault(adjacentPoint, 9) > height);
                });

        private HashSet<GridPoint> ExpandBasin(IDefinedSizeGrid<int> grid, GridPoint lowestPoint)
        {
            var pointsInBasin = new HashSet<GridPoint> { lowestPoint };

            // Let's do a sort of breadth first search starting from the lowest point
            var pointsToInvestigate = new Queue<GridPoint>();
            pointsToInvestigate.Enqueue(lowestPoint);
            while (pointsToInvestigate.Any())
            {
                // Choose a point we haven't investigated and try expanding the basin around it.
                var expandedPoints = pointsToInvestigate
                    .Dequeue()
                    .Adjacent4
                    .Where(p =>
                        grid.GetOrDefault(p, 9) != 9 &&
                        // But let's not expand backwards.
                        !pointsInBasin.Contains(p));

                foreach (var candidatePoint in expandedPoints)
                {
                    pointsInBasin.Add(candidatePoint);
                    pointsToInvestigate.Enqueue(candidatePoint);
                }
            }

            return pointsInBasin;
        }
    }
}