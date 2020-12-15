using System.Linq;

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
            throw new System.NotImplementedException();
        }
    }
}