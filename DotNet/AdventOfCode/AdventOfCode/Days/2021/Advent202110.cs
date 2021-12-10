using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Versioning;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2021
{
    public class Advent202110 : Problem
    {
        public override object PartOne(string[] input)
        {
            var results = input.Select(ExamineSyntaxForProblems).ToList();
            return results.Where(c => c.CorruptedChar is not null)
                .Select(c => GetCorruptionScore(c.CorruptedChar.Value)).Sum();
        }

        public override object PartTwo(string[] input)
        {
            var scores = input
                .Select(ExamineSyntaxForProblems)
                .Where(x => x.RemainingChars.Any())
                .Select(x => GetCorrectionScore(x.RemainingChars))
                .OrderBy(x => x)
                .ToList();

            return scores[scores.Count / 2];
        }

        private int GetCorruptionScore(char c) => c switch
        {
            ')' => 3,
            ']' => 57,
            '}' => 1197,
            '>' => 25137
        };

        private (char? CorruptedChar, List<char> RemainingChars) ExamineSyntaxForProblems(string line)
        {
            var stack = new Stack<char>();
            foreach (var c in line)
            {
                char? closerForOpener = c switch
                {
                    '(' => ')',
                    '[' => ']',
                    '<' => '>',
                    '{' => '}',
                    _ => null
                };

                if (closerForOpener is not null)
                {
                    stack.Push(closerForOpener.Value);
                }
                else
                {
                    var correctCloser = stack.Pop();
                    if (c != correctCloser)
                    {
                        return (c, new List<char>());
                    }
                }
            }

            return (null, stack.ToList());
        }

        private long GetCorrectionScore(IEnumerable<char> chars)
        {
            var sum = 0L;
            foreach (var c in chars)
            {
                var charScore = c switch
                {
                    ')' => 1,
                    ']' => 2,
                    '}' => 3,
                    '>' => 4
                };

                sum *= 5;
                sum += charScore;
            }

            return sum;
        }
    }
}