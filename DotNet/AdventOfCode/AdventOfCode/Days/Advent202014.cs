using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days
{
    public class Advent202014 : Problem
    {
        public Advent202014() : base(2020, 14)
        {
        }

        public override string PartOne(string[] input)
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

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}