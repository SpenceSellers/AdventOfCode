using System;
using System.Linq;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days
{
    public class Advent202012 : Problem
    {
        public Advent202012() : base(2020, 12)
        {
        }

        public override string PartOne(string[] input)
        {
            var ship = new ShipState();
            foreach (var (ins, arg) in input.Select(ParseInstruction))
            {
                ship.Apply(ins, arg);
            }

            return ship.Position.ManhattanDistanceFromOrigin().ToString();
        }

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }

        private (char, int) ParseInstruction(string s) => (s[0], int.Parse(s.Substring(1)));

        private class ShipState
        {
            public GridPoint Position = new GridPoint(0, 0);
            public int Facing;

            private void ApplyRotation(int degrees)
            {
                // Hacky way to support the type of negative modulo we need
                Facing += degrees + 360;
                Facing %= 360;
            }

            private void MoveForward(int distance)
            {
                var direction = Facing switch
                {
                    0 => 'E',
                    90 => 'S',
                    180 => 'W',
                    270 => 'N',
                    _ => throw new Exception($"Invalid current facing: {Facing}")
                };
                
                Apply(direction, distance);
            }

            public void Apply(char instruction, int argument)
            {
                switch (instruction)
                {
                    case 'N':
                        Position += new GridPoint(0, 1).Scale(argument);
                        break;
                    case 'S':
                        Position += new GridPoint(0, -1).Scale(argument);
                        break;
                    case 'E':
                        Position += new GridPoint(1, 0).Scale(argument);
                        break;
                    case 'W':
                        Position += new GridPoint(-1, 0).Scale(argument);
                        break;
                    case 'R':
                        ApplyRotation(argument);
                        break;
                    case 'L':
                        ApplyRotation(-argument);
                        break;
                    case 'F':
                        MoveForward(argument);
                        break;
                }
            }
        }
    }
}