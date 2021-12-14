using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2021
{
    public class Advent202114 : Problem
    {
        public override object PartOne(string[] input)
        {
            var initial = input[0].ToCharArray();
            var rules = input[2..]
                .Select(s => new Regex("(..) -> (.)")
                    .Captures(s)
                    .Two())
                .ToDictionary();

            var currentState = initial.ToList();

            for (int i = 0; i < 10; i++)
            {
                currentState = DoStep(currentState, rules);
            }

            var histogram = currentState.Histogram();
            return histogram.Values.Max() - histogram.Values.Min();
        }

        private List<char> DoStep(IEnumerable<char> initial, Dictionary<string, string> rules)
        {
            var next = new List<char> { initial.First() };
            foreach (var pair in initial.SequencesOfSize(2))
            {
                var pairString = $"{pair[0]}{pair[1]}";
                // next.Add(pair[0]);
                if (rules.TryGetValue(pairString, out var newChar))
                {
                    next.Add(newChar[0]);
                }
                next.Add(pair[1]);
            }

            return next;
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}