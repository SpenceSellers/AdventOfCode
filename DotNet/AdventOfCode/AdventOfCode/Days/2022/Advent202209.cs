using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2022;

public class Advent202209 : Problem
{
    public override object PartOne(string[] input)
    {
        var movements = input.Select(ParseMovement).ToList();
        var head = GridPoint.Origin;
        var tail = GridPoint.Origin;
        var visted = new HashSet<GridPoint> { tail };
        foreach (var (gridDirection, distance) in movements)
        {
            for (int i = 0; i < distance; i++)
            {
                var offset = gridDirection.AsUnitPoint(GridInterpretation.Math);
                head += offset;
                if (!AreTouching(head, tail))
                {
                    tail = MoveTail(head, tail);
                }

                visted.Add(tail);
                Console.Out.WriteLine($"{head}, {tail}");
            }
        }

        return visted.Count;
    }

    private GridPoint MoveTail(GridPoint head, GridPoint tail)
    {
        return tail + (head - tail).UnitAxes;
    }

    private bool AreTouching(GridPoint head, GridPoint tail)
    {
        var delta = head - tail;
        return Math.Abs(delta.X) <= 1 && Math.Abs(delta.Y) <= 1;
    }

    private record Movement(GridDirection Direction, int Distance);

    private Movement ParseMovement(string input)
    {
        var (sDirection, sDistance) = input.Split(" ").Two();
        var direction = sDirection switch
        {
            "D" => GridDirection.Down,
            "U" => GridDirection.Up,
            "L" => GridDirection.Left,
            "R" => GridDirection.Right
        };

        return new Movement(direction, int.Parse(sDistance));
    }

    public override object PartTwo(string[] input)
    {
        throw new System.NotImplementedException();
    }
}