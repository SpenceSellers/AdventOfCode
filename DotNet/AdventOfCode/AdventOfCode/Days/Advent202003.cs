using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days
{
    public class Advent202003 : Problem
    {
        public Advent202003() : base(2020, 03)
        {
        }

        public override object PartOne(string[] input)
        {
            return TreesForSlope(input, new GridPoint(3, 1)).ToString();
        }

        public override object PartTwo(string[] input)
        {
            var slopes = new[]
            {
                new GridPoint(1, 1),
                new GridPoint(3, 1),
                new GridPoint(5, 1),
                new GridPoint(7, 1),
                new GridPoint(1, 2)
            };

            return slopes
                .Select(s => new BigInteger(TreesForSlope(input, s)))
                .Product()
                .ToString();
        }

        private static int TreesForSlope(string[] input, GridPoint slope)
        {
            var patternWillRepeatTimes = (input.Length - 1) / slope.Y;
            return SolidGrid<char>.Extract(input)
                .Map(c => c == '#')
                .Wrapping()
                .Warp(p => new GridPoint(p.X * slope.X, p.Y * slope.Y)) // Squish the world to just trees aligned to our slope.
                .Warp(p => new GridPoint(p.X + p.Y, p.Y)) // Turn diagonals into verticals
                .Windowed(new GridRegion(GridPoint.Origin, 1, patternWillRepeatTimes + 1)) // Focus on the now-vertical path we walked down
                .AllCells()
                .Count(t => t);
        }
    }
}