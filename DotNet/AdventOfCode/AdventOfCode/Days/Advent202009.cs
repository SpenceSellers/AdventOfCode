using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202009 : Problem
    {
        public Advent202009() : base(2020, 9)
        {
        }

        public override string PartOne(string[] input)
        {
            var nums = input.Select(long.Parse).ToList();
            for (var i = 25; i < nums.Count; i++)
            {
                var candidates = nums.GetRange(i - 25, 25);
                if (!CanBeMadeFrom(nums[i], candidates))
                {
                    return nums[i].ToString();
                }
            }

            return null;
        }

        private static bool CanBeMadeFrom(long n, IList<long> nums)
        {
            return nums.Combinations(2).Any(a =>
            {
                var b = a.ToArray();
                return b[0] + b[1] == n;
            });
        }

        public override string PartTwo(string[] input)
        {
            var nums = input.Select(long.Parse).ToList();

            // Don't talk to me about the duplicate work or the poor choice of type. Cycles are cheap and it's baked into the interface, OK?
            var targetNumber = long.Parse(PartOne(input));

            for (var i = 2;; i++)
            {
                var range = nums.SequencesOfSize(i).FirstOrDefault(seq => seq.Sum() == targetNumber);
                if (range != null)
                {
                    return (range.Min() + range.Max()).ToString();
                }
            }
        }
    }
}