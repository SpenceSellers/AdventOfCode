using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202002 : Problem
    {
        public Advent202002() : base(2020, 02)
        {
        }

        public override string PartOne(string[] input)
        {
            var passwords = input.Select(ParseLine);
            return passwords.Count(p => p.IsValidOne()).ToString();
        }
        
        public override string PartTwo(string[] input)
        {
            var passwords = input.Select(ParseLine);
            return passwords.Count(p => p.IsValidTwo()).ToString();
        }

        private class PasswordLine
        {
            public int Min;
            public int Max;
            public char Char;
            public string Password;

            public bool IsValidOne()
            {
                var count = Password.ToCharArray().Count(c => c == Char);
                return count <= Max && count >= Min;
            }

            public bool IsValidTwo()
            {
                var first = Password[Min - 1] == Char ? 1 : 0;
                var second = Password[Max - 1] == Char ? 1 : 0;
                return first + second == 1;
            }
        }

        private static PasswordLine ParseLine(string l)
        {
            var pieces = l.Replace("-", " ").Replace(":", "").Split();
            return new PasswordLine
            {
                Min = int.Parse(pieces[0]),
                Max = int.Parse(pieces[1]),
                Char = pieces[2][0],
                Password = pieces[3]
            };
        }
    }
}