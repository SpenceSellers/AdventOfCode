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
            var dangerSpots = grid.Region().AllPoints().Where(point =>
            {
                var height = grid.Get(point);
                return point.Adjacent4
                    .Where(adjacentPoint => grid.Region().ContainsPoint(adjacentPoint))
                    .All(adjacentPoint => grid.Get(adjacentPoint) > height);
            });

            return dangerSpots
                .Select(x => grid.Get(x) + 1)
                .Sum();
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}