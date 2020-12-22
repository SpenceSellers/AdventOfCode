using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202005 : Problem
    {
        public Advent202005() : base(2020, 05)
        {
        }

        public override object PartOne(string[] input)
        {
            return SeatIds(input).Max.ToString();
        }
        
        public override object PartTwo(string[] input)
        {
            var seatIds = SeatIds(input);
            var ourSeatId = seatIds.First(id => !seatIds.Contains(id + 1)) + 1;
            return ourSeatId.ToString();
        }

        private ImmutableSortedSet<int> SeatIds(IEnumerable<string> input) => input.Select(SeatId).ToImmutableSortedSet();
        
        private int SeatId(string s) => ParseRow(s) * 8 + ParseSeat(s);

        
        private static int ParseRow(string s)
        {
            var binary = s.Substring(0, 7).Replace('F', '0').Replace('B', '1');
            return Convert.ToInt32(binary, 2);
        }

        private static int ParseSeat(string s)
        {
            var binary = s.Substring(7, 3).Replace('R', '1').Replace('L', '0');
            return Convert.ToInt32(binary, 2);
        }
    }
}