using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days._2021
{
    public class Advent202103 : Problem
    {
        public override object PartOne(string[] input)
        {
            var bits = input.Select(x => x.ToCharArray()).ToList();
            var length = input[0].Length;
            var commonBits = Transpose(length, bits)
                .Select(column =>
                {
                    var ones = column.Count(x => x == '1');
                    return ones > (bits.Count / 2) ? '1' : '0';
                }).ToList();

            var a =  Convert.ToInt32(string.Join("", commonBits), 2);
            var b =  Convert.ToInt32(Invert(string.Join("", commonBits)), 2);
            return a * b;
        }

        private static IEnumerable<IEnumerable<char>> Transpose(int length, List<char[]> bits)
        {
            for (var i = 0; i < length; i++)
            {
                var column = bits.Select(x => x[i]).ToList();
                yield return column;
            }
        }

        public static string Invert(string s)
        {
            return string.Join("", s.ToCharArray().Select(x => x == '1' ? '0' : '1'));
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}