using System;
using System.Linq;
using System.Numerics;
using System.Text;

namespace AdventOfCode.Days
{
    public class Advent202013 : Problem
    {
        public Advent202013() : base(2020, 13)
        {
        }

        public override string PartOne(string[] input)
        {
            var initialTime = int.Parse(input[0]);
            var busses = input[1].Split(",");
            var activeBusses = busses
                .Where(b => b != "x")
                .Select(int.Parse)
                .ToList();

            var (timeToNext, idOfNext) = activeBusses
                .Select(bus => bus - (initialTime % bus))
                .Zip(activeBusses)
                .OrderBy(x => x.First)
                .FirstOrDefault();

            return (timeToNext * idOfNext).ToString();
        }

        public override string PartTwo(string[] input)
        {
            var busses = input[1]
                .Split(",")
                .Select((b, i) => (b, i))
                .Where(x => x.b != "x")
                .Select(x => (int.Parse(x.b), x.i))
                .ToList();

            // Are ya winning, son?
            var sb = new StringBuilder();
            sb.AppendLine("So what you're going to do is go to this website:");
            sb.AppendLine("https://www.dcode.fr/chinese-remainder");
            sb.AppendLine("It's a pretty cute site. You'll then plug in these numbers:");
            sb.AppendJoin('\n', busses.Select(x => $"[{x.Item1 - (x.i % x.Item1)}, {x.Item1}]"));
            sb.AppendLine();
            sb.AppendLine("That'll give you the answer.");

            return sb.ToString();
        }

    }
}