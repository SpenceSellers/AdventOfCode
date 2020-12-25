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
                var pickedUp = new List<int> {cups[1], cups[2], cups[3]};
                cups.RemoveRange(1, 3);
            
                var destinationIndex = FindIndexOfNextLowest(cups, cups[0]);
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
            return null;
        }

        public override object PartTwo(string[] input)
        {
            var cups = input[0].ToCharArray().Select(c => int.Parse(c.ToString())).ToList();
            for (int i = 10; i <= 1_000_000; i++)
            {
                cups.Add(i);
            }
            
            Console.Out.WriteLine($"Len: {cups.Count}");

            var moves = 10_000_000;

            for (var i = 0; i < moves; i++)
            {
                var pickedUp = new List<int> {cups[1], cups[2], cups[3]};
                cups.RemoveRange(1, 3);

                var destinationIndex = FindIndexOfNextLowest(cups, cups[0]);
                cups.InsertRange(destinationIndex + 1, pickedUp);
                var currentCup = cups[0];
                cups.RemoveAt(0);
                cups.Add(currentCup);
                if (i % 10_000 == 0)
                {
                    var ratio = ((double) i) / moves;
                    Console.Out.WriteLine($"{i} ({ratio * 100}%)");
                }
            }

            var oneIndex = cups.IndexOf(1);
            var indexA = (oneIndex + 1) % cups.Count;
            var indexB = (oneIndex + 2) % cups.Count;
            var aa = (long) cups[indexA];
            var bb = (long) cups[indexB];
            Console.Out.WriteLine($"{aa} * {bb}");
            return aa * bb;
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
                if (n == -1) n = cups.Count + 1;
                var idx = cups.IndexOf(n);
                if (idx >= 0) return idx;
                n--;
            }
        }
    }
}