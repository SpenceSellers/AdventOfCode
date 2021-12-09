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
            var grid = input.ToGrid().Map(x => int.Parse(x.ToString()));
            var dangerSpots = FindLowestPoints(grid);

            return dangerSpots
                .Select(x => grid.Get(x) + 1)
                .Sum();
        }

        public override object PartTwo(string[] input)
        {
            var grid = input.ToGrid().Map(x => int.Parse(x.ToString()));

            return FindLowestPoints(grid)
                .Select(lowest => ExpandBasin(grid, lowest))
                .Select(basin => basin.Count)
                .OrderByDescending(size => size)
                .Take(3)
                .Product();
        }

        private static IEnumerable<GridPoint> FindLowestPoints(IDefinedSizeGrid<int> grid)
        {
            return grid
                .Region()
                .AllPoints()
                .Where(point =>
                {
                    var height = grid.Get(point);
                    return point.Adjacent4
                        .All(adjacentPoint => grid.GetOrDefault(adjacentPoint, 9) > height);
                });
        }

        private HashSet<GridPoint> ExpandBasin(IDefinedSizeGrid<int> grid, GridPoint lowestPoint)
        {
            var pointsInBasin = new HashSet<GridPoint> { lowestPoint };

            var pointsToInvestigate = new Queue<GridPoint>();
            pointsToInvestigate.Enqueue(lowestPoint);
            while (pointsToInvestigate.Any())
            {
                var candidatePoints = pointsToInvestigate
                    .Dequeue()
                    .Adjacent4
                    .Where(p =>
                        grid.GetOrDefault(p, 9) != 9 &&
                        !pointsInBasin.Contains(p));

                foreach (var candidatePoint in candidatePoints)
                {
                    pointsInBasin.Add(candidatePoint);
                    pointsToInvestigate.Enqueue(candidatePoint);
                }
            }

            return pointsInBasin;
        }
    }
}