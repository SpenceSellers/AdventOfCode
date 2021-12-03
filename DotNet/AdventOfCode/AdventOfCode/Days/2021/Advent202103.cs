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
            var commonBits = CommonBits(bits);

            var a =  Convert.ToInt32(string.Join("", commonBits), 2);
            var b =  Convert.ToInt32(Invert(string.Join("", commonBits)), 2);
            return a * b;
        }

        private static List<char> CommonBits(List<char[]> bits)
        {
            var length = bits[0].Length;
            var commonBits = Transpose(length, bits)
                .Select(column =>
                {
                    var ones = column.Count(x => x == '1');
                    var zeroes = column.Count(x => x == '0');
                    return ones >= zeroes ? '1' : '0';
                }).ToList();
            return commonBits;
        }

        private static IEnumerable<IEnumerable<char>> Transpose(int length, List<char[]> bits)
        {
            for (var i = 0; i < length; i++)
            {
                var column = bits.Select(x => x[i]).ToList();
                yield return column;
            }
        }

        private static string Invert(string s)
        {
            return string.Join("", s.ToCharArray().Select(x => x == '1' ? '0' : '1'));
        }

        public override object PartTwo(string[] input)
        {
            var bits = input.Select(x => x.ToCharArray()).ToList();
            var originalBits = bits;

            for (var i = 0; i < bits[0].Length; i++)
            {
                var commonBits = CommonBits(bits);
                var criticalBit = commonBits[i];
                bits = bits.Where(c => c[i] == criticalBit).ToList();
                if (bits.Count == 1)
                {
                    break;
                }
            }

            if (bits.Count != 1)
            {
                throw new Exception("Uh oh " + bits.Count);
            }
            var aaa = Convert.ToInt32(string.Join("", bits[0]), 2);

            bits = originalBits;
            for (var i = 0; i < bits[0].Length; i++)
            {
                var commonBits = CommonBits(bits);
                var criticalBit = commonBits[i] == '1' ? '0' : '1';
                bits = bits.Where(c => c[i] == criticalBit).ToList();
                if (bits.Count == 1)
                {
                    break;
                }
            }

            if (bits.Count != 1)
            {
                throw new Exception("Uh oh " + bits.Count);
            }

            var bbb = Convert.ToInt32(string.Join("", bits[0]), 2);
            return aaa * bbb;
        }
    }
}