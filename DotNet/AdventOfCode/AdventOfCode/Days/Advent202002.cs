using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202002 : Problem
    {
        public Advent202002() : base(2020, 02)
        {
        }

        public override object PartOne(string[] input) =>
            input
                .Select(ParseLine)
                .Count(p => p.IsValidByCount());

        public override object PartTwo(string[] input) =>
            input
                .Select(ParseLine)
                .Count(p => p.IsValidByPosition());

        private record PasswordLine
        {
            public int Min;
            public int Max;
            public char Char;
            public string Password;

            public bool IsValidByCount()
            {
                var count = Password.ToCharArray().Count(c => c == Char);
                return count <= Max && count >= Min;
            }

            public bool IsValidByPosition()
            {
                var first = Password[Min - 1] == Char;
                var second = Password[Max - 1] == Char;
                return first ^ second;
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