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

        public override string PartOne(string[] input)
        {
            return TreesForSlope(input, new GridPoint(3, 1)).ToString();
        }

        public override string PartTwo(string[] input)
        {
            var slopes = new[]
            {
                new GridPoint(1, 1),
                new GridPoint(3, 1),
                new GridPoint(5, 1),
                new GridPoint(7, 1),
                new GridPoint(1, 2)
            };

            return slopes.Select(s => new BigInteger(TreesForSlope(input, s))).Product().ToString();
        }

        private static int TreesForSlope(IEnumerable<string> input, GridPoint slope)
        {
            var cells = input.Select(line => line.ToCharArray().Select(x => x == '#').ToArray()).ToArray();
            var width = cells[0].Length;
            var pos = new GridPoint(0, 0);
            var treeCount = 0;
            while (pos.Y < cells.Length)
            {
                var tree = cells[pos.Y][pos.X % width];
                if (tree)
                {
                    treeCount++;
                }

                pos += slope;
            }

            return treeCount;
        }
    }
}