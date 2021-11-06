using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2020
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
            var hexPlane = BuildInitialState(input);

            return hexPlane.Values.Count(c => c);
        }

        public override object PartTwo(string[] input)
        {
            var hexPlane = BuildInitialState(input);

            for (int i = 0; i < 100; i++)
            {
                var nextStep = new Dictionary<(int, int), bool>();
                var toConsider = hexPlane.Where(kv => kv.Value).Select(kv => kv.Key).SelectMany(PositionsToConsider).ToHashSet();
                foreach (var pos in toConsider)
                {
                    var current = hexPlane.GetDefault(pos, false);
                    var adjacent = AdjacentPositions(pos)
                        .Select(adjPos => hexPlane.GetDefault(adjPos, false))
                        .Count(x => x);

                    var next = current switch
                    {
                        false => adjacent == 2,
                        true => adjacent is not (0 or > 2)
                    };

                    nextStep[pos] = next;
                }

                hexPlane = nextStep;
            }
            
            return hexPlane.Values.Count(c => c);
        }

        private IEnumerable<(int, int)> PositionsToConsider((int, int) pos)
        {
            yield return pos;
            foreach (var adjacentPosition in AdjacentPositions(pos))
            {
                yield return adjacentPosition;
            }
        }

        private IEnumerable<(int, int)> AdjacentPositions((int, int) pos)
        {
            var directions = new[]
            {
                HexDirection.West, HexDirection.East, HexDirection.SouthEast, HexDirection.SouthWest,
                HexDirection.NorthEast, HexDirection.NorthWest
            };

            return directions.Select(d => ApplyDirection(pos, d));
        }

        private Dictionary<(int, int), bool> BuildInitialState(string[] input)
        {
            var instructions = input.Select(ParseLine);
            var hexPlane = new Dictionary<(int, int), bool>();
            foreach (var hexDirections in instructions)
            {
                var currentPos = (0, 0);
                foreach (var hexDirection in hexDirections)
                {
                    currentPos = ApplyDirection(currentPos, hexDirection);
                }

                hexPlane.UpdateWithDefault(currentPos, false, b => !b);
            }

            return hexPlane;
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