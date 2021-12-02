using System.Linq;

namespace AdventOfCode.Days._2021
{
    public class Advent202102 : Problem
    {
        public override object PartOne(string[] input)
        {
            var instructions = input.Select(x => x.Split()).Select(x => (x[0], int.Parse(x[1])));
            var x = 0;
            var y = 0;
            foreach (var (direction, distance) in instructions)
            {
                switch (direction)
                {
                    case "forward":
                        x += distance;
                        break;
                    case "down":
                        y += distance;
                        break;
                    case "up":
                        y -= distance;
                        break;
                }
            }

            return x * y;
        }

        public override object PartTwo(string[] input)
        {
            var instructions = input.Select(x => x.Split()).Select(x => (x[0], int.Parse(x[1])));
            var x = 0;
            var y = 0;
            var aim = 0;
            foreach (var (direction, distance) in instructions)
            {
                switch (direction)
                {
                    case "forward":
                        x += distance;
                        y += distance * aim;
                        break;
                    case "down":
                        aim += distance;
                        break;
                    case "up":
                        aim -= distance;
                        break;
                }
            }

            return x * y;
        }
    }
}