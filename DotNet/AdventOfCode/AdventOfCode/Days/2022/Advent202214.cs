using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;
using AdventOfCode.AdventLib.Grid.SparseGrids;

namespace AdventOfCode.Days._2022;

public class Advent202214 : Problem
{
    public override object PartOne(string[] input)
    {
        var startingPoint = new GridPoint(500, 0);
        var (grid, maxY) = ParseProblem(input);

        var count = 0;
        while (true)
        {
            var settling = SettlingPoint(startingPoint, grid, maxY, true);
            if (settling == null)
            {
                break;
            }

            grid.Set(settling.Value, new Sand());
            count++;
        }

        grid.AsDefinedSizeNotPreservingCoordinates().Map(x => x switch
        {
            Rock _ => '█',
            Sand _ => '░',
            _ => ' '
        }).Trace();

        return count;
    }

    public override object PartTwo(string[] input)
    {
        var startingPoint = new GridPoint(500, 0);
        var (grid, maxY) = ParseProblem(input);

        var count = 0;
        while (true)
        {
            var settling = SettlingPoint(startingPoint, grid, maxY, false);
            if (settling == startingPoint)
            {
                // 1 is for the starting point.
                count++;
                break;
            }

            grid.Set(settling.Value, new Sand());
            count++;
        }

        grid.AsDefinedSizeNotPreservingCoordinates().Map(x => x switch
        {
            Rock _ => '█',
            Sand _ => '░',
            _ => ' '
        }).Trace();

        return count;

    }

    private (SparseGrid<ICell> grid, int maxY) ParseProblem(string[] input)
    {
        var parsedInput = input.Select(x => ParseLine(x).ToList()).ToList();
        var grid = ParseWalls(parsedInput);
        var region = grid.BoundingRegion();
        var maxY = region.Origin.Y + region.Height;
        return (grid, maxY);
    }

    private readonly GridPoint[] _fallOffsets = { new(0, 1), new(-1, 1), new(1, 1) };

    private GridPoint? SettlingPoint(GridPoint start, SparseGrid<ICell> grid, int maxY, bool voiding)
    {
        var currentPoint = start;
        while (true)
        {
            if (currentPoint.Y >= maxY)
            {
                return voiding ? null : currentPoint; // We're fallen blow maxY
            }
            var couldMove = false;
            foreach (var fallOffset in _fallOffsets)
            {
                var possibleNext = currentPoint + fallOffset;
                if (!grid.ContainsPoint(possibleNext))
                {
                    // We will go this way
                    currentPoint = possibleNext;
                    couldMove = true;
                    break;
                }
            }

            if (!couldMove)
            {
                return currentPoint;
            }
        }
    }

    private SparseGrid<ICell> ParseWalls(IEnumerable<IEnumerable<GridPoint>> paths)
    {
        var grid = new SparseGrid<ICell>();
        foreach (var path in paths)
        {
            foreach (var (start, end) in path.SequencesOfSize(2))
            {
                foreach (var gridPoint in GridPoint.PointsBetween(start, end))
                {
                    grid.Set(gridPoint, new Rock());
                }
            }
        }

        return grid;
    }

    private interface ICell
    {
    }

    private record Rock : ICell;

    private record Sand : ICell;

    private IEnumerable<GridPoint> ParseLine(string line)
    {
        return line.Split(" -> ").Select(coord =>
        {
            var (x, y) = coord.Split(",").Select(int.Parse).Two();
            return new GridPoint(x, y);
        });
    }
}