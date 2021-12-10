using System.Collections.Generic;
using System.Linq;
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

        public char? FirstIncorrectChar(string line)
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

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}