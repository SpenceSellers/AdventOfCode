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
        var parsedInput = input.Select(x => ParseLine(x).ToList()).ToList();
        var grid = ParseWalls(parsedInput);
        var region = grid.BoundingRegion();
        var maxY = region.Origin.Y + region.Height + 10; // The 10 is for pure paranoia
        var startingPoint = new GridPoint(500, 0);

        var count = 0;
        while (true)
        {
            var settling = SettlingPoint(startingPoint, grid, maxY);
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

    private GridPoint[] FallOffsets = { new(0, 1), new(-1, 1), new(1, 1) };
    private GridPoint? SettlingPoint(GridPoint start, SparseGrid<ICell> grid, int maxY)
    {
        var currentPoint = start;
        while (true)
        {
            if (currentPoint.Y >= maxY)
            {
                return null; // We're fallen blow maxY
            }
            var couldMove = false;
            foreach (var fallOffset in FallOffsets)
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

    public override object PartTwo(string[] input)
    {
        throw new System.NotImplementedException();
    }
}