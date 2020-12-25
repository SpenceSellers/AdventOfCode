using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202024 : Problem
    {
        private enum HexDirection
        {
            East,
            West,
            SouthEast,
            SouthWest,
            NorthEast,
            NorthWest
        }
        
        public override object PartOne(string[] input)
        {
            var hexPlane = new Dictionary<(int, int), bool>();
            var instructions = input.Select(ParseLine);
            foreach (var hexDirections in instructions)
            {
                var currentPos = (0, 0);
                foreach (var hexDirection in hexDirections)
                {
                    currentPos = ApplyDirection(currentPos, hexDirection);
                }
                hexPlane.UpdateWithDefault(currentPos, false, b => !b);
            }

            return hexPlane.Values.Count(c => c);
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }

        private (int, int) ApplyDirection((int, int) coord, HexDirection direction)
        {
            var (x, y) = coord;
            return direction switch
            {
                HexDirection.West => (x - 1, y),
                HexDirection.East => (x + 1, y),
                HexDirection.NorthEast => (x + 1, y - 1),
                HexDirection.NorthWest => (x, y - 1),
                HexDirection.SouthEast => (x, y + 1),
                HexDirection.SouthWest => (x - 1, y + 1),
            };
        }

        private IEnumerable<HexDirection> ParseLine(string line)
        {
            var chars = new Queue<char>(line.ToCharArray());
            while (chars.Any())
            {
                yield return chars.Dequeue() switch
                {
                    'e' => HexDirection.East,
                    'w' => HexDirection.West,
                    'n' => chars.Dequeue() switch
                    {
                        'e' => HexDirection.NorthEast,
                        'w' => HexDirection.NorthWest
                    },
                    's' => chars.Dequeue() switch
                    {
                        'e' => HexDirection.SouthEast,
                        'w' => HexDirection.SouthWest
                    }
                };
            }

        }
    }
}