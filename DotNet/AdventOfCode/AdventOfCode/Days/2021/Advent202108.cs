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
            return ParseInput(input)
                .SelectMany(line =>
                {
                    var examples = line[0];
                    var digits = line[1];
                    var mapping = BuildSimplestMappings(examples);
                    return digits.Select(d => mapping.GetValueOrDefault(d, -1));
                }).Count(d => d is 1 or 4 or 7 or 8);
        }

        private List<List<List<string>>> ParseInput(string[] input)
        {
            var parsed = input
                .Select(line => line.Split('|', StringSplitOptions.TrimEntries)
                    .Select(side => side
                        .Split()
                        .Select(NormalizeString)))
                .Realize();
            return parsed;
        }

        private string NormalizeString(string s)
        {
            return string.Join("", s.ToCharArray().OrderBy(x => x));
        }

        private Dictionary<string, int> BuildSimplestMappings(List<string> scrambledDigits)
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


        private readonly Dictionary<string, int> _digitTable = new()
        {
            {"abcefg", 0},
            {"cf", 1},
            {"acdeg", 2},
            {"acdfg", 3},
            {"bcdf", 4},
            {"abdfg", 5},
            {"abdefg", 6},
            {"acf", 7},
            {"abcdefg", 8},
            {"abcdfg", 9},
        };

        // We're going to need this 200 times, let's cache it.
        private readonly Lazy<List<Dictionary<char, char>>> AllPossibleMappingsCache = new(() => AllPossibleMappings().ToList());

        public override object PartTwo(string[] input)
        {
            return ParseInput(input).Select((line, i) =>
            {
                var examples = line[0];
                var digits = line[1];
                var mapping = BruteForceMapping(examples);
                var solved = digits.Select(d => _digitTable[NormalizeString(ApplyMapping(mapping, d))]);
                return int.Parse(string.Join("", solved.Select(i => i.ToString())));
            }).Sum();
        }

        private Dictionary<char, char> BruteForceMapping(List<string> examples)
        {
            return AllPossibleMappingsCache.Value
                .Where(mapping => MappingIsPossible(mapping, examples))
                .ToList().First();
        }
        
        // Yep, we're seriously going to brute force everything the answer could be.
        private static IEnumerable<Dictionary<char, char>> AllPossibleMappings() {
            return "abcdefg"
                .ToList()
                .Permutations()
                .Select(scrambled => "abcdefg"
                    .Zip(scrambled)
                    .ToDictionary(k => k.Second, v => v.First));
        }

        private string ApplyMapping(Dictionary<char, char> mapping, string scrambled)
        {
            return string.Join("", scrambled.Select(c => mapping[c]));
        }

        private bool MappingIsPossible(Dictionary<char, char> mapping, List<string> examples)
        {
            return examples
                .Select(example => NormalizeString(ApplyMapping(mapping, example)))
                .All(mapped => _digitTable.ContainsKey(mapped));
        }
    }
}