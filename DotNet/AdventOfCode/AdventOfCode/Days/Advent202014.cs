using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days
{
    public class Advent202014 : Problem
    {
        public Advent202014() : base(2020, 14)
        {
        }

        public override object PartOne(string[] input)
        {
            long mask0 = 0;
            long mask1 = 0;
            var values = new Dictionary<long, long>();
            var lineRegex = new Regex(@"^(.*?) = (.*)$");
            var memRegex = new Regex(@"mem\[(\d+)\]");
            foreach (var line in input)
            {
                var match = lineRegex.Match(line);
                var lvalue = match.Groups[1].Value;
                var rvalue = match.Groups[2].Value;

                var memGroup = memRegex.Match(lvalue);
                if (memGroup.Success)
                {
                    var masked = long.Parse(rvalue) & mask0 | mask1;
                    values[long.Parse(memGroup.Groups[1].Value)] = masked;
                }
                else
                {
                    mask0 = Convert.ToInt64(rvalue.Replace("X", "1"), 2);
                    mask1 = Convert.ToInt64(rvalue.Replace("X", "0"), 2);
                }
            }

            return values.Values.Sum().ToString();
        }

        public override object PartTwo(string[] input)
        {
            long mask1 = 0;
            long maskX = 0;
            var values = new Dictionary<long, long>();
            var lineRegex = new Regex(@"^(.*?) = (.*)$");
            var memRegex = new Regex(@"mem\[(\d+)\]");
            foreach (var line in input)
            {
                var match = lineRegex.Match(line);
                var lvalue = match.Groups[1].Value;
                var rvalue = match.Groups[2].Value;

                var memGroup = memRegex.Match(lvalue);
                if (memGroup.Success)
                {
                    var addr = long.Parse(memGroup.Groups[1].Value);
                    var masked = addr | mask1;
                    foreach (var address in MaskOptions(masked, maskX))
                    {
                        values[address] = long.Parse(rvalue);
                    }
                }
                else
                {
                    mask1 = Convert.ToInt64(rvalue.Replace("X", "0"), 2);
                    maskX = Convert.ToInt64(rvalue.Replace("1", "0").Replace("X", "1"), 2);
                }
            }

            return values.Values.Sum().ToString();
        }

        public IEnumerable<long> MaskOptions(long target, long allMask)
        {
            // I really wanted to do this with bit fiddling but by head hurts
            var positions = PositionsFromMask(allMask);
            for (long i = 0; i < 1 << positions.Count; i++)
            {
                yield return Emblazon(target, i, positions);
            }
        }

        private static List<int> PositionsFromMask(long allMask)
        {
            var positions = new List<int>();
            for (int j = 0; j <= 40; j++)
            {
                var mask = 1L << j;
                if ((mask & allMask) != 0)
                {
                    positions.Add(j);
                }
            }

            return positions;
        }

        public static long Emblazon(long target, long source, IEnumerable<int> positions)
        {
            var sourceIndex = 0;
            foreach (var targetIndex in positions)
            {
                var sourceMask = 1L << sourceIndex;
                // exactly 0 or 1
                var sourceBit = (source & sourceMask) >> sourceIndex;
                // 0000100000
                var targetMask = 1L << targetIndex;
                // Blank out that bit on the target
                target &= ~targetMask;
                
                // Optionally fill it back in with a 1
                if (sourceBit != 0)
                {
                    target |= targetMask;
                }

                sourceIndex++;
            }

            return target;
        }
        
    }
}