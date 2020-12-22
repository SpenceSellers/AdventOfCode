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

        public override object PartOne(string[] input)
        {
            var adapters = input.Select(int.Parse);
            var remainingAdapters = adapters.ToList();
            var adapterStack = new List<int>();
            
            // This is seriously equivalent to sorting the list. Except I read
            // the whole description and implemented the whole thing, and THEN
            // I realized that it's freaking list.Sort().
            //
            // It stays here as a testament to my ignorance and hard work.
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

        private IDictionary<int, long> memoizedCounts = new Dictionary<int, long>();

        public override object PartTwo(string[] input)
        {
            var adapters = input.Select(int.Parse).ToHashSet();
            return ValidArrangements(adapters.Max(), 0, adapters).ToString();
        }

        private long ValidArrangements(int targetJoltage, int sourceJoltage, HashSet<int> adapters)
        {
            if (targetJoltage == sourceJoltage)
            {
                // We have found one single valid way to get to our target joltage.
                return 1;
            }

            // So, this gets me the right answer but I'm not fully satisfied with why it's safe.
            // It appears that given a set of adapters, a current joltage, and a target joltage,
            // there's a single number of ways to get to the target voltage... No matter what
            // adapters you've used up so far. To me it seems like it'd be possible to
            // use up one of the adapters we need and throw off the memoization, but I tried this
            // memoization key in desperation and it worked for my input, at least.
            var memoizedKey = sourceJoltage;
            if (memoizedCounts.ContainsKey(memoizedKey))
            {
                // We already know how to solve this one
                return memoizedCounts[memoizedKey];
            }

            var answer = adapters.Where(ad => AdapterCanFit(ad, sourceJoltage)).Select(adapter =>
                {
                    var removed = adapters.Where(a => a != adapter).ToHashSet();
                    return ValidArrangements(targetJoltage, adapter, removed);
                })
                .Sum();

            memoizedCounts.Add(memoizedKey, answer);
            return answer;
        }
    }
}