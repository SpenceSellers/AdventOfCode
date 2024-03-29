using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2020
{
    public class Advent202001 : Problem
    {
        public Advent202001() : base(2020, 01)
        {
        }
        
        public override object PartOne(string[] input)
        {
            return SolveFor(input, 2);
        }

        public override object PartTwo(string[] input)
        {
            return SolveFor(input, 3);
        }
        
        private static string SolveFor(IEnumerable<string> input, int n) =>
            input
                .Select(int.Parse)
                .Combinations(n)
                .First(pairs => pairs.Sum() == 2020)
                .Product()
                .ToString();
    }
}