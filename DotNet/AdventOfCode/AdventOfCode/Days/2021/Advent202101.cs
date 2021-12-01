using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2021
{
    public class Advent202101 : Problem
    {
        public override object PartOne(string[] input)
        {
            return CountAscending(input.Select(int.Parse));
        }

        public override object PartTwo(string[] input)
        {
            var depthSums = input
                .Select(int.Parse)
                .SequencesOfSize(3)
                .Select(d => d.Sum());
            return CountAscending(depthSums);
        }

        private static int CountAscending(IEnumerable<int> depths)
        {
            int? previous = null;
            var count = 0;
            foreach (var i in depths)
            {
                if (previous != null && i > previous)
                {
                    count++;
                }

                previous = i;
            }

            return count;
        }
    }
}