using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202010 : Problem
    {
        public Advent202010() : base(2020, 10)
        {
        }

        public override string PartOne(string[] input)
        {
            var adapters = input.Select(int.Parse);
            var remainingAdapters = adapters.ToList();
            var adapterStack = new List<int>();
            while (remainingAdapters.Any())
            {
                var candidates = remainingAdapters
                    .Where(adapter => AdapterCanFit(adapter, adapterStack.LastOrDefault()))
                    .ToList();
                var chosen = candidates.Min();
                Console.Out.WriteLine($"Choosing {chosen}");
                remainingAdapters.Remove(chosen);
                adapterStack.Add(chosen);
            }
            
            // Add source
            adapterStack.Insert(0, 0);
            // Add device
            adapterStack.Add(adapterStack.Last() + 3);

            var differences = adapterStack.SequencesOfSize(2).Select(a => a[1] - a[0]).ToList();
            var diff1 = differences.Count(d => d == 1);
            var diff3 = differences.Count(d => d == 3);
            return (diff1 * diff3).ToString();
        }

        public bool AdapterCanFit(int adapter, int joltage)
        {
            var difference = adapter - joltage;
            // Zero is not a valid difference
            return difference <= 3 && difference >= 1;
        }

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}