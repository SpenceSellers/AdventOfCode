using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AdventOfCode.Days
{
    public class Advent202023 : Problem

    {
        public override object PartOne(string[] input)
        {
            var cups = input[0].ToCharArray().Select(c => int.Parse(c.ToString())).ToList();

            var moves = 100;

            for (var i = 0; i < moves; i++)
            {
                Console.Out.WriteLine(ShowCups(cups));
                var pickedUp = new List<int> {cups[1], cups[2], cups[3]};
                Console.Out.WriteLine("Picked up:"+ ShowCups(pickedUp));
                cups.RemoveRange(1, 3);
                Console.Out.WriteLine(ShowCups(cups));

                var destinationIndex = FindIndexOfNextLowest(cups, cups[0]);
                Console.Out.WriteLine($"Destination: {cups[destinationIndex]} @ {destinationIndex}");
                cups.InsertRange(destinationIndex + 1, pickedUp);
                var currentCup = cups[0];
                cups.RemoveAt(0);
                cups.Add(currentCup);
            }

            var oneIndex = cups.IndexOf(1);
            var sb = new StringBuilder();
            for (int i = 1; i < 9; i++)
            {
                var index = (oneIndex + i) % cups.Count;
                sb.Append(cups[index]);
            }

            return sb.ToString();
        }

        private string ShowCups(IList<int> numbers)
        {
            return string.Join(" ", numbers.Select(x => x.ToString()));
        }

        private int FindIndexOfNextLowest(IList<int> cups, int n)
        {
            n--;
            while (true)
            {
                if (n == -1) n = 9;
                var idx = cups.IndexOf(n);
                if (idx >= 0) return idx;
                n--;
            }
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}