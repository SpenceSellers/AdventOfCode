using System;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202018 : Problem
    {
        public Advent202018() : base(2020, 18)
        {
        }

        public override string PartOne(string[] input)
        {
            return input.Select(i => SolveLine(i)).Sum().ToString();
        }

        public long SolveLine(string expr)
        {
            var accumulate = 0L;
            var nextOperation = '?';
            var i = 0;
            while (i < expr.Length)
            {
                var c = expr[i];

                if (char.IsDigit(c))
                {
                    var value = int.Parse(c.ToString());
                    accumulate = nextOperation switch
                    {
                        '?' => value,
                        '+' => accumulate + value,
                        '*' => accumulate * value
                    };
                }

                if (c == '*' || c == '+')
                {
                    nextOperation = c;
                }

                if (c == '(')
                {
                    var matchingIndex = FindMatch(expr, i);
                    var subExpr = expr.Substring(i+1, matchingIndex - i - 1);
                    var value = SolveLine(subExpr);
                    accumulate = nextOperation switch
                    {
                        '?' => value,
                        '+' => accumulate + value,
                        '*' => accumulate * value
                    };

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

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}