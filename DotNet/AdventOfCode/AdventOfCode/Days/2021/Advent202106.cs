using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2021
{
    public class Advent202106 : Problem
    {
        public override object PartOne(string[] input)
        {
            return Solve(input, 80);
        }

        public override object PartTwo(string[] input)
        {
            return Solve(input, 256);
        }

        private object Solve(string[] input, int steps)
        {
            var fish = input[0].Split(",")
                .Select(long.Parse)
                .ToList();

            var fishFacts = new Dictionary<long, long>();
            foreach (var f in fish)
            {
                fishFacts.Increment(f);
            }

            for (var i = 0; i < steps; i++)
            {
                fishFacts = DoStep(fishFacts);
            }

            return fishFacts.Values.Sum();
        }

        public IEnumerable<long> FishFactors(long fish)
        {
            if (fish == 0)
            {
                yield return 6;
                yield return 8;
            }
            else
            {
                yield return fish - 1;
            }
        }

        public Dictionary<long, long> DoStep(Dictionary<long, long> initialFish)
        {
            var output = new Dictionary<long, long>();
            foreach (var (f, count) in initialFish)
            {
                foreach (var newFish in FishFactors(f))
                {
                    output.Increment(newFish, count);
                }
            }

            return output;
        }

    }
}