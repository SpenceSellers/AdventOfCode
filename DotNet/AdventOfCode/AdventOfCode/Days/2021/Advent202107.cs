using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2021
{
    public class Advent202107 : Problem
    {
        public override object PartOne(string[] input)
        {
            var a = input[0].Split(",").Select(int.Parse).ToList();
            var max = a.Max();
            var min = a.Min();

            return InclusiveRange(min, max).Select(i => CostToConvergePart1(a, i)).Min();
        }

        private static IEnumerable<int> InclusiveRange(int min, int max)
        {
            for (var i = min; i <= max; i++)
            {
                yield return i;
            }
        }

        private int CostToConvergePart1(IEnumerable<int> crabs, int destination)
        {
            return crabs.Select(crab => Math.Abs(crab - destination)).Sum();
        }

        public override object PartTwo(string[] input)
        {
            var a = input[0].Split(",").Select(int.Parse).ToList();
            var max = a.Max();
            var min = a.Min();


            return InclusiveRange(min, max).Select(i => CostToConvergePart2(a, i)).Min();
        }

        private readonly Dictionary<int, int> _arithmeticSumCache = new();

        private int CostToMove2(int crab, int destination)
        {
            var dist = Math.Abs(crab - destination);
            // I know there's an equation to get this in a closed-form solution. I tried.
            // I googled THREE different variants of the equation. Never figured it out. I give up.
            return _arithmeticSumCache.Caching(dist, _ => Enumerable.Range(1, dist).Sum());
        }

        private int CostToConvergePart2(IEnumerable<int> crabs, int destination)
        {
            return crabs.Select(crab => CostToMove2(crab, destination)).Sum();
        }
    }
}