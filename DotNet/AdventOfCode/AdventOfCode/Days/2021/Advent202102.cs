using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days._2021
{
    public class Advent202102 : Problem
    {
        public override object PartOne(string[] input)
        {
            var horizontal = 0;
            var depth = 0;
            PilotSubmarine(input, new Dictionary<string, Action<int>>
            {
                {"forward", x => horizontal += x},
                {"down", x => depth += x},
                {"up", x => depth -= x}
            });

            return horizontal * depth;
        }

        public override object PartTwo(string[] input)
        {
            var horizontal = 0;
            var depth = 0;
            var aim = 0;
            PilotSubmarine(input, new Dictionary<string, Action<int>>
            {
                {"forward", x =>
                    {
                        horizontal += x;
                        depth += x * aim;
                    }
                },
                {"down", x => aim += x},
                {"up", x => aim -= x}
            });

            return horizontal * depth;
        }

        private void PilotSubmarine(string[] input, Dictionary<string, Action<int>> handlers)
        {
            var instructions = Parse(input);
            foreach (var (direction, distance) in instructions)
            {
                handlers[direction](distance);
            }
        }

        private static IEnumerable<(string, int)> Parse(string[] input)
        {
            var instructions = input
                .Select(x => x.Split())
                .Select(x => (x[0], int.Parse(x[1])));
            return instructions;
        }
    }
}