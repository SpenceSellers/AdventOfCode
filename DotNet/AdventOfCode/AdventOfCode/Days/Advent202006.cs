using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202006 : Problem
    {
        public Advent202006() : base(2020, 06)
        {
        }

        public override string PartOne(string[] input)
        {
            return ParseQuestions(input).Select(answers => answers.Keys.Count).Sum().ToString();
        }

        public override string PartTwo(string[] input)
        {
            return ParseQuestions2(input).Select(group =>
            {
                var intersected = group.First();
                foreach (var person in group.Skip(1))
                {
                    intersected.IntersectWith(person);
                }

                return intersected;
            }).Select(group => group.Count).Sum().ToString();
        }
        
        private static IEnumerable<Dictionary<char, int>> ParseQuestions(IEnumerable<string> lines)
        {
            var answers = new Dictionary<char, int>();
            foreach (var line in lines)
            {
                if (line == "")
                {
                    // We're at the end of a group
                    yield return answers;
                    answers = new Dictionary<char, int>();
                }
                else
                {
                    foreach (var c in line)
                    {
                        IncrementDict(answers, c);
                    }
                }
            }

            // Spit out the last passport even if there was no blank line
            if (answers.Any())
            {
                yield return answers;
            }
        }
        
        private static IEnumerable<IList<ISet<char>>> ParseQuestions2(IEnumerable<string> lines)
        {
            var group = new List<ISet<char>>();
            foreach (var line in lines)
            {
                if (line == "")
                {
                    // We're at the end of a group
                    yield return group;
                    group = new List<ISet<char>>();
                }
                else
                {
                    group.Add(line.ToHashSet());
                }
            }

            // Spit out the last passport even if there was no blank line
            if (group.Any())
            {
                yield return group;
            }
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