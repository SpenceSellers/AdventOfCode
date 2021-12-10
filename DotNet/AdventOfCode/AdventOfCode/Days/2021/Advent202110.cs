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
            var results = input.Select(FirstIncorrectChar).ToList();
            return results.Where(c => c is not null).Select(c => GetScore(c.Value)).Sum();
        }

        public int GetScore(char c) => c switch
        {
            ')' => 3,
            ']' => 57,
            '}' => 1197,
            '>' => 25137
        };

        private char? FirstIncorrectChar(string line)
        {
            var stack = new Stack<char>();
            foreach (var c in line)
            {
                char? closer = c switch
                {
                    '(' => ')',
                    '[' => ']',
                    '<' => '>',
                    '{' => '}',
                    _ => null
                };

                if (closer is not null)
                {
                    stack.Push(closer.Value);
                }
                else
                {
                    var correctCloser = stack.Pop();
                    if (c != correctCloser)
                    {
                        return c;
                    }
                }
            }

            return null;
        }

        private List<char> FirstIncorrectChar2(string line)
        {
            var stack = new Stack<char>();
            foreach (var c in line)
            {
                char? closer = c switch
                {
                    '(' => ')',
                    '[' => ']',
                    '<' => '>',
                    '{' => '}',
                    _ => null
                };

                if (closer is not null)
                {
                    stack.Push(closer.Value);
                }
                else
                {
                    var correctCloser = stack.Pop();
                    if (c != correctCloser)
                    {
                        return null;
                    }
                }
            }

            return stack.ToList();
        }

        private long ScorePart2(IEnumerable<char> chars)
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

        public override object PartTwo(string[] input)
        {
            var scores = input
                .Select(FirstIncorrectChar2)
                .Where(x => x != null)
                .Select(ScorePart2)
                .OrderBy(x => x)
                .ToList();

            return scores[scores.Count / 2];
        }
    }
}