using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AdventOfCode.AdventLib;
using Microsoft.VisualBasic;

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
                var h = currentState.Histogram();

                // Console.Out.WriteLine($"{i}::: K is {h['K']}, O is {h['O']}, difference is {h['K'] - h['O']}");
                Console.Out.WriteLine($"{i}, {h['K'] - h['O']}");

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
            var initial = input[0].ToCharArray();
            var rules = input[2..]
                .Select(s => new Regex("(..) -> (.)")
                    .Captures(s)
                    .Two())
                .ToDictionary();

            var pairs = initial
                .SequencesOfSize(2)
                .Select(x => $"{x[0]}{x[1]}")
                .Histogram();

            var substitutionRules = rules.ToDictionary(k => k.Key, v => new[] { v.Key[0] + v.Value, v.Value + v.Key[1] });
            var current = pairs;
            for (var i = 0; i < 40; i++)
            {
                current = DoStep2(current, substitutionRules);
            }

            var histogram = new Dictionary<char, long>();
            foreach (var (k, v) in current)
            {
                histogram.Increment(k[0], v);
            }
            // We're not considering the very last character, let's add it in
            histogram.Increment(initial[^1]);

            return histogram.Values.Max() - histogram.Values.Min();
        }

        private Dictionary<string, long> DoStep2(
            Dictionary<string, long> current,
            Dictionary<string, string[]> substitutionRules)
        {
            var nextStep = new Dictionary<string, long>();
            foreach (var (pair, count) in current)
            {
                if (substitutionRules.ContainsKey(pair))
                {
                    var rules = substitutionRules[pair];
                    foreach (var replacement in rules)
                    {
                        nextStep.UpdateWithDefault(replacement, 0, i => i + count);
                    }
                }
                else
                {
                    nextStep.UpdateWithDefault(pair, 0, i => i + 1);
                }
            }

            return nextStep;
        }
    }
}