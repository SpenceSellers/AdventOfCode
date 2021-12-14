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
            return SolveDay(input, 10);
        }

        public override object PartTwo(string[] input)
        {
            return SolveDay(input, 40);
        }

        private object SolveDay(string[] input, int n)
        {
            var initial = input[0].ToCharArray();
            var substitutionRules = ParseSubstitutionRules(input);

            // Change the way we think about the input from characters, to the count of PAIRS of characters
            var pairs = initial
                .SequencesOfSize(2)
                .Select(x => $"{x[0]}{x[1]}")
                .Histogram();

            var current = pairs;
            for (var i = 0; i < n; i++)
            {
                current = DoStep(current, substitutionRules);
            }

            var histogram = CountLettersFromPairs(current, initial);

            return histogram.Values.Max() - histogram.Values.Min();
        }

        private static Dictionary<string, string[]> ParseSubstitutionRules(string[] input)
        {
            var rawRules = input[2..]
                .Select(s => new Regex("(..) -> (.)")
                    .Captures(s)
                    .Two())
                .ToDictionary();

            // Build substitution rules that output PAIRS of characters, not individual characters
            return rawRules
                .ToDictionary(k => k.Key, v => new[] { v.Key[0] + v.Value, v.Value + v.Key[1] });
        }

        private static Dictionary<char, long> CountLettersFromPairs(Dictionary<string, long> current, char[] initial)
        {
            var histogram = new Dictionary<char, long>();
            foreach (var (k, v) in current)
            {
                histogram.Increment(k[0], v);
            }

            // We're not considering the very last character, let's add it in
            histogram.Increment(initial[^1]);
            return histogram;
        }

        private Dictionary<string, long> DoStep(
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
                    // We don't have a substitution rule for this, so let's keep it the same
                    nextStep.UpdateWithDefault(pair, 0, i => i + 1);
                }
            }

            return nextStep;
        }
    }
}