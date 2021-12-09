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

        private static IEnumerable<GridPoint> FindLowestPoints(IDefinedSizeGrid<int> grid)
        {
            return grid
                .Region()
                .AllPoints()
                .Where(point =>
                {
                    var height = grid.Get(point);
                    return point.Adjacent4
                        .Where(adjacentPoint => grid.Region().ContainsPoint(adjacentPoint))
                        .All(adjacentPoint => grid.Get(adjacentPoint) > height);
                });
        }

        public override object PartTwo(string[] input)
        {
            var grid = input.ToGrid().Map(x => int.Parse(x.ToString()));
            var lowestPoints = FindLowestPoints(grid);
            var basins = lowestPoints.Select(lowest => ExpandBasin(grid, lowest))
                .ToList();

            return basins.Select(basin => basin.Count).OrderByDescending(size => size).Take(3).Product();
        }

        private HashSet<GridPoint> ExpandBasin(IDefinedSizeGrid<int> grid, GridPoint lowestPoint)
        {
            var pointsInBasin = new HashSet<GridPoint>();
            pointsInBasin.Add(lowestPoint);

            var pointsToInvestigate = new Queue<GridPoint>();
            pointsToInvestigate.Enqueue(lowestPoint);
            while (pointsToInvestigate.Any())
            {
                var currentPoint = pointsToInvestigate.Dequeue();
                var candidatePoints = currentPoint.Adjacent4.Where(p => grid.Region().ContainsPoint(p));

                foreach (var candidatePoint in candidatePoints)
                {
                    if (grid.Get(candidatePoint) != 9)
                    {
                        var wasAdded = pointsInBasin.Add(candidatePoint);
                        if (wasAdded)
                        {
                            pointsToInvestigate.Enqueue(candidatePoint);
                        }
                    }
                }
            }

            return pointsInBasin;
        }
    }
}