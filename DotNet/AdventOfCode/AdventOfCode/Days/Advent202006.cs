using System;
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

        public override object PartOne(string[] input)
        {
            return GroupCount(input, (a, b) =>
            {
                a.UnionWith(b);
                return a;
            }).ToString();
        }

        public override object PartTwo(string[] input)
        {
            return GroupCount(input, (a, b) =>
            {
                a.IntersectWith(b);
                return a;
            }).ToString();
        }

        private static int GroupCount(IEnumerable<string> input, Func<ISet<char>, ISet<char>, ISet<char>> aggregator) =>
            ParseQuestions(input)
                .Select(group => group.Aggregate(aggregator).Count)
                .Sum();

        private static IEnumerable<IEnumerable<ISet<char>>> ParseQuestions(IEnumerable<string> lines)
        {
            return new SeparatedGroupParser()
                .Parse(lines)
                .Select(group => group
                    .Select(person => person.ToHashSet()));
        }
    }
}