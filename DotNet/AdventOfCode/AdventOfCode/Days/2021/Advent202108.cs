using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2021
{
    public class Advent202108 : Problem
    {
        public override object PartOne(string[] input)
        {
            var parsed = input
                .Select(line => line.Split('|', StringSplitOptions.TrimEntries)
                    .Select(side => side
                        .Split()
                        .Select(NormalizeString)))
                .Realize();

            return parsed.SelectMany(line =>
            {
                var examples = line[0];
                var digits = line[1];
                var mapping = BuildMapping(examples);
                return digits.Select(d => mapping.GetValueOrDefault(d, -1));
            }).Count(d => d is 1 or 4 or 7 or 8);
        }

        public string NormalizeString(string s)
        {
            return string.Join("", s.ToCharArray().OrderBy(x => x));
        }

        public Dictionary<string, int> BuildMapping(List<string> scrambledDigits)
        {
            var one = scrambledDigits.Single(s => s.Length == 2);
            var four = scrambledDigits.Single(s => s.Length == 4);
            var seven = scrambledDigits.Single(s => s.Length == 3);
            var eight = scrambledDigits.Single(s => s.Length == 7);
            var mapping = new Dictionary<string, int>
            {
                {one, 1},
                {four, 4},
                {seven, 7},
                {eight, 8}
            };
            return mapping;
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}