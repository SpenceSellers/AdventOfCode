using System.Linq;
using System.Text;

namespace AdventOfCode.Days._2020
{
    public class Advent202013 : Problem
    {
        public Advent202013() : base(2020, 13)
        {
        }

        public override object PartOne(string[] input)
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

            return timeToNext * idOfNext;
        }

        public override object PartTwo(string[] input)
        {
            var busses = input[1]
                .Split(",")
                // Remember the indexes before we nuke the broken busses.
                .Select((bus, index) => (bus, index))
                .Where(x => x.bus != "x")
                .Select(x => (Bus: int.Parse(x.bus!), Index: x.index))
                .ToList();

            // Are ya winning, son?
            var sb = new StringBuilder();
            sb.AppendLine("So what you're going to do is go to this website:");
            sb.AppendLine("https://www.dcode.fr/chinese-remainder");
            sb.AppendLine("It's a pretty cute site. You'll then plug in these numbers:");
            sb.AppendJoin('\n', busses.Select(x => $"{x.Bus - (x.Index % x.Bus)}\t{x.Bus}"));
            sb.AppendLine();
            sb.AppendLine("That'll give you the answer.");

            return sb.ToString();
        }

    }
}