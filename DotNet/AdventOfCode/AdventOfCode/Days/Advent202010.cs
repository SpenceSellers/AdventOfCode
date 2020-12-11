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

        private IDictionary<string, long> _memoized = new Dictionary<string, long>();

        public override string PartTwo(string[] input)
        {
            var adapters = input.Select(int.Parse).ToList();
            var phoneValue = adapters.Max() + 3;
            return ValidArrangements(phoneValue - 3, 0, adapters.ToHashSet()).ToString();
        }

        private long ValidArrangements(int targetJoltage, int sourceJoltage, HashSet<int> adapters)
        {
            if (targetJoltage == sourceJoltage)
            {
                return 1;
            }

            var memoizedKey = $"{sourceJoltage};{string.Join(',', adapters.OrderBy(a => a))}";
            Console.Out.WriteLine($"Trying {memoizedKey}");
            if (_memoized.ContainsKey(memoizedKey))
            {
                Console.Out.WriteLine("HIT memoization");
                return _memoized[memoizedKey];
            }

            var answer =  adapters.Where(ad => AdapterCanFit(ad, sourceJoltage)).Select(adapter =>
                {
                    var removed = adapters.Where(a => a != adapter).ToHashSet();
                    return ValidArrangements(targetJoltage, adapter, removed);
                })
                .Sum();
            
            _memoized.Add(memoizedKey, answer);
            return answer;
        }
    }
}