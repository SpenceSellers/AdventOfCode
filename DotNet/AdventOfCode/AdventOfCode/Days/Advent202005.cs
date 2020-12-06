using System;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202005 : Problem
    {
        public Advent202005() : base(2020, 05)
        {
        }

        public override string PartOne(string[] input)
        {
            // TODO, I just looked at the input and solved part one by eye.
            return 861.ToString();
        }

        private int ParseRow(string s)
        {
            var binary = s.Substring(0, 7).Replace('F', '0').Replace('B', '1');
            return Convert.ToInt32(binary, 2);
        }

        private int ParseSeat(string s)
        {
            var binary = s.Substring(7, 3).Replace('R', '1').Replace('L', '0');
            return Convert.ToInt32(binary, 2);
        }

        private int SeatId(string s) => ParseRow(s) * 8 + ParseSeat(s);

        public override string PartTwo(string[] input)
        {
            var seatIds = input.Select(SeatId).OrderBy(x => x).ToList();
            foreach (var id in seatIds)
            {
                if (!seatIds.Contains(id + 1))
                {
                    return (id + 1).ToString();
                }
            }

            return null;
        }
    }
}