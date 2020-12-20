using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AdventOfCode.Days
{
    public class Advent202018 : Problem
    {
        public override string PartOne(string[] input)
        {
            return input.Select(i => SolveLine(i)).Sum().ToString();
        }
        
        public override string PartTwo(string[] input)
        {
            return input.Select(i => SolveLine2(i)).ToList().Sum().ToString();
        }

        private long SolveLine(string expr)
        {
            var accumulate = 0L;
            var nextOperation = '?';
            var i = 0;

            void EncounterNumber(long value)
            {
                accumulate = nextOperation switch
                {
                    '?' => value,
                    '+' => accumulate + value,
                    '*' => accumulate * value
                };
            }
            
            while (i < expr.Length)
            {
                var c = expr[i];

                if (char.IsDigit(c))
                {
                    var value = int.Parse(c.ToString());
                    EncounterNumber(value);
                }

                if (c is '*' or '+')
                {
                    nextOperation = c;
                }

                if (c is '(')
                {
                    var matchingIndex = FindMatch(expr, i);
                    var subExpr = expr.Substring(i+1, matchingIndex - i - 1);
                    EncounterNumber(SolveLine(subExpr));

                    // Jump to char after the closing paren
                    i = matchingIndex + 1;
                }

                i++;
            }

            return accumulate;
        }

        private int FindMatch(string expr, int start)
        {
            var count = 0;
            for (var i = start;; i++)
            {
                var c = expr[i];
                switch (c)
                {
                    case '(':
                        count++;
                        break;
                    case ')':
                        count--;
                        break;
                }

                if (count == 0)
                {
                    return i;
                }
            }
        }

        private long SolveLine2(string expr)
        {
            var flat = KillParens(expr, s => SolveLine2(s).ToString());
            var additionExprs = flat.Split('*');

            var pieceParts = additionExprs
                .Select(additionOnly => additionOnly
                    .Split('+')
                    .Select(s => s.Trim())
                    .Select(long.Parse)
                    .Sum());
            return pieceParts.Aggregate(1L, (a, b) => a * b);
        }

        private string KillParens(string s, Func<string, string> parenSquisher)
        {
            var stringBuilder = new StringBuilder();
            for (var i = 0; i < s.Length; i++)
            {
                if (s[i] == '(')
                {
                    var matchingIndex = FindMatch(s, i);
                    var subExpr = s.Substring(i+1, matchingIndex - i - 1);
                    stringBuilder.Append(parenSquisher(subExpr));
                    i = matchingIndex;
                }
                else
                {
                    stringBuilder.Append(s[i]);
                }
            }

            return stringBuilder.ToString();
        }

    }
}