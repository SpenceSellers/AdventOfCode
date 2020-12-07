using System.Collections.Generic;
using System.Linq;
using AdventOfCode.Parsing;

namespace AdventOfCode.Days
{
    public class Advent202006 : Problem
    {
        public Advent202006() : base(2020, 06)
        {
        }

        public override string PartOne(string[] input)
        {
            return ParseQuestions(input).Select(group => group.Aggregate((a, b) =>
            {
                a.UnionWith(b);
                return a;
            }))
                .Select(group => group.Count)
                .Sum()
                .ToString();
        }

        public override string PartTwo(string[] input)
        {
            return ParseQuestions(input).Select(group => group.Aggregate((a, b) =>
            {
                a.IntersectWith(b);
                return a;
            }))
                .Select(group => group.Count)
                .Sum()
                .ToString();
        }
        
        private static IEnumerable<IEnumerable<ISet<char>>> ParseQuestions(IEnumerable<string> lines)
        {
            return new SeparatedGroupParser()
                .Parse(lines)
                .Select(group => group
                    .Select(person => person.ToHashSet()));
        }

        private static void IncrementDict<T>(IDictionary<T, int> dict, T key, int count = 1)
        {
            if (dict.ContainsKey(key))
            {
                dict[key] += count;
            }
            else
            {
                dict[key] = count;
            }
        }
    }
}