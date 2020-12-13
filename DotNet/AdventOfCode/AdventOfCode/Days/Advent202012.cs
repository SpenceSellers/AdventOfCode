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
            var ship = new Part1ShipState();
            return SolvePart(ship, input).ToString();
        }

        public override string PartTwo(string[] input)
        {
            var ship = new Part2ShipState();
            return SolvePart(ship, input).ToString();
        }

        private int SolvePart(ShipState ship, string[] input)
        {
            foreach (var (ins, arg) in input.Select(ParseInstruction))
            {
                ship.Apply(ins, arg);
            }

            return ship.Position.ManhattanDistanceFromOrigin();
        }

        private (char, int) ParseInstruction(string s) => (s[0], int.Parse(s.Substring(1)));

        private abstract class ShipState
        {
            public GridPoint Position = new GridPoint(0, 0);
            public abstract void Apply(char instruction, int argument);
        }

        private class Part1ShipState : ShipState
        {
            private int _facing;

            private void ApplyRotation(int degrees)
            {
                // Hacky way to support the type of negative modulo we need
                _facing += degrees + 360;
                _facing %= 360;
            }

            private void MoveForward(int distance)
            {
                var direction = _facing switch
                {
                    0 => 'E',
                    90 => 'S',
                    180 => 'W',
                    270 => 'N',
                    _ => throw new Exception($"Invalid current facing: {_facing}")
                };

                Apply(direction, distance);
            }

            public override void Apply(char instruction, int argument)
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

        private class Part2ShipState : ShipState
        {
            public GridPoint WaypointPosition = new GridPoint(10, 1);
            // public int Facing;

            private void ApplyRotation(int degrees)
            {
                WaypointPosition = WaypointPosition.RotateAroundOrigin(degrees / 90);
            }

            private void MoveForward(int distance)
            {
                Position += WaypointPosition.Scale(distance);
            }

            public override void Apply(char instruction, int argument)
            {
                switch (instruction)
                {
                    case 'N':
                        WaypointPosition += new GridPoint(0, 1).Scale(argument);
                        break;
                    case 'S':
                        WaypointPosition += new GridPoint(0, -1).Scale(argument);
                        break;
                    case 'E':
                        WaypointPosition += new GridPoint(1, 0).Scale(argument);
                        break;
                    case 'W':
                        WaypointPosition += new GridPoint(-1, 0).Scale(argument);
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