using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2022;

public class Advent202209 : Problem
{
    public override object PartOne(string[] input)
    {
        var movements = input.Select(ParseMovement).ToList();
        var visited = SimulateKnots(2, movements);

        return visited.Count;
    }

    public override object PartTwo(string[] input)
    {
        var movements = input.Select(ParseMovement).ToList();
        var visited = SimulateKnots(10, movements);

        return visited.Count;
    }

    private HashSet<GridPoint> SimulateKnots(int numKnots, List<Movement> movements)
    {
        var knots = new GridPoint[numKnots];
        for (int i = 0; i < knots.Length; i++)
        {
            knots[i] = GridPoint.Origin;
        }

        var visited = new HashSet<GridPoint> { GridPoint.Origin };
        foreach (var (directionToMove, distanceToMove) in movements)
        {
            for (var i = 0; i < distanceToMove; i++)
            {
                // Move the head according to the instructions
                var offset = directionToMove.AsUnitPoint(GridInterpretation.Math);
                knots[0] += offset;

                // And now move the rest of the knots
                UpdateFollowingKnots(knots);

                visited.Add(knots.Last());
            }
        }

        return visited;
    }

    private void UpdateFollowingKnots(GridPoint[] knots)
    {
        for (var knotIndex = 1; knotIndex < knots.Length; knotIndex++)
        {
            ref var currentKnot = ref knots[knotIndex];
            var higherKnot = knots[knotIndex - 1];
            if (!AreTouching(higherKnot, currentKnot))
            {
                currentKnot = MoveTail(higherKnot, currentKnot);
            }
        }
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
}