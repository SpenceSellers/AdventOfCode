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
                var mapping = BuildSimplestMappings(examples);
                return digits.Select(d => mapping.GetValueOrDefault(d, -1));
            }).Count(d => d is 1 or 4 or 7 or 8);
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

        private void ResolveMappings(List<string> scrambled)
        {

            var resolver = new LetterResolver();
            // Seed the possible mappings by knocking out the ones we KNOW can't be true
            foreach (var (simpleMapping, i) in BuildSimplestMappings(scrambled))
            {
                resolver.KnownCharacter(simpleMapping, i);
            }
        }

        public IEnumerable<Dictionary<char, char>> AllPossibleMappings()
        {
            return "abcdefg"
                .ToList()
                .Permutations()
                .Select(scrambled => "abcdefg"
                    .Zip(scrambled)
                    .ToDictionary(k => k.Second, v => v.First));
        }

        private Dictionary<int, string> _digitTable = new()
        {
            {0, "abcefg"},
            {1, "cf"},
            {2, "acdeg"},
            {3, "acdfg"},
            {4, "bcdf"},
            {5, "abdfg"},
            {6, "abdefg"},
            {7, "acf"},
            {8, "abcdefg"},
            {9, "abcdfg"},
        };

        private Dictionary<string, int> _inverseDigitTable = new()
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

        public override object PartTwo(string[] input)
        {
            var parsed = input
                .Select(line => line.Split('|', StringSplitOptions.TrimEntries)
                    .Select(side => side
                        .Split()
                        .Select(NormalizeString)))
                .Realize();

            return parsed.Select((line, i) =>
            {
                Console.Out.WriteLine($"Line {i}");
                var examples = line[0];
                var digits = line[1];
                var mapping = DetermineMapping(examples);
                var solved = digits.Select(d => _inverseDigitTable[NormalizeString(ApplyMapping(mapping, d))]);
                return int.Parse(string.Join("", solved.Select(i => i.ToString())));
            }).Sum();
        }

        private Dictionary<char, char> DetermineMapping(List<string> examples)
        {
            return AllPossibleMappings()
                .Where(mapping => MappingIsPossible(mapping, examples))
                .ToList().First();
        }

        public string ApplyMapping(Dictionary<char, char> mapping, string scrambled)
        {
            return string.Join("", scrambled.Select(c => mapping[c]));
        }

        public bool MappingIsPossible(Dictionary<char, char> mapping, List<string> examples)
        {
            var mapped = examples
                .Select(example => ApplyMapping(mapping, example))
                .Select(mapped => _inverseDigitTable.GetValueOrDefault(NormalizeString(mapped), -1))
                .Where(x => x is not -1)
                .ToList();
            return mapped.ToHashSet().Count == 10;
        }
    }

    // I literally gave up on doing it the right way
    public class LetterResolver
    {
        private Dictionary<int, string> _digitTable = new()
        {
            {0, "abcefg"},
            {1, "cf"},
            {2, "acdeg"},
            {3, "acdfg"},
            {4, "bcdf"},
            {5, "abdfg"},
            {6, "abdefg"},
            {7, "acf"},
            {8, "abcdefg"},
            {9, "abcdfg"},
        };

        // Maps Scrambled characters <-> Real characters
        private Dictionary<char, List<char>> _possibilities = new();

        public LetterResolver()
        {
            foreach (var c in "abcdefg")
            {
                _possibilities[c] = "abcdefg".ToList();
            }
        }

        public void KnownCharacter(string scrambled, int n)
        {
            var realDigits = _digitTable[n];
            foreach (var scrambledChar in scrambled.ToList())
            {
                _possibilities[scrambledChar].RemoveAll(c => !realDigits.Contains(c));
            }
        }

        // public IEnumerable<Dictionary<char, char>> AllPossibleMappings(List<char> decided = null)
        // {
        //     decided ??= new List<char>();
        //
        // }
    }
}