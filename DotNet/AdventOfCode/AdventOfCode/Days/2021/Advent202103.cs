using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace AdventOfCode.Days._2021
{
    public class Advent202103 : Problem
    {
        public override object PartOne(string[] input)
        {
            var bits = input.Select(x => x.ToCharArray()).ToList();
            var commonBits = CommonBits(bits);

            var gamma = ToInt(commonBits);
            var epsilon =  ToInt(Invert(commonBits));
            return gamma * epsilon;
        }

        public override object PartTwo(string[] input)
        {
            var bits = input.Select(x => x.ToCharArray()).ToList();
            var oxygen = ToInt(ReduceBits(bits, false));
            var co2 = ToInt(ReduceBits(bits, true));
            return oxygen * co2;
        }

        private static List<char> CommonBits(IList<char[]> bits) =>
            Transpose(bits)
                .Select(column =>
                {
                    var ones = column.Count(x => x == '1');
                    return ones >= (bits.Count + 1) / 2 ? '1' : '0';
                }).ToList();

        /// <summary>
        /// Rotate the bits on their side so we don't have to think sideways
        /// </summary>
        private static IEnumerable<List<char>> Transpose(IList<char[]> bits)
        {
            var length = bits[0].Length;
            for (var i = 0; i < length; i++)
            {
                var column = bits.Select(x => x[i]).ToList();
                yield return column;
            }
        }

        private static IEnumerable<char> Invert(IEnumerable<char> l) => l.Select(Invert);
        private static char Invert(char c) => c == '1' ? '0' : '1';

        private static int ToInt(IEnumerable<char> bits) => Convert.ToInt32(string.Join("", bits), 2);

        private static IEnumerable<char> ReduceBits(List<char[]> bits, bool invert)
        {
            for (var i = 0; i < bits[0].Length; i++)
            {
                var commonBits = CommonBits(bits);
                var criticalBit = commonBits[i];
                criticalBit = invert ? Invert(criticalBit) : criticalBit;
                bits = bits.Where(c => c[i] == criticalBit).ToList();
                if (bits.Count == 1)
                {
                    return bits[0];
                }
            }

            throw new InvalidOperationException("There are more than one candidate bitstrings left");
        }
    }
}