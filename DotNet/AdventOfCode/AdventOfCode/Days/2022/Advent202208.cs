using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2022;

public class Advent202208 : Problem
{
    public override object PartOne(string[] input)
    {
        var grid = input.ToGrid().Map(x => int.Parse(x.ToString()));

        var visible = new SolidGrid<bool>(grid.Region().Width, grid.Region().Width, false);

        for (var row = 0; row < grid.Height; row++)
        {
            var maxSoFar = -1;
            for (var x = 0; x < grid.Width; x++)
            {
                var point = new GridPoint(x, row);
                var heightHere = grid.Get(point);
                if (heightHere > maxSoFar)
                {
                    visible.Set(point, true);
                    maxSoFar = heightHere;
                }
            }
        }

        for (var row = 0; row < grid.Height; row++)
        {
            var maxSoFar = -1;
            for (var x = 0; x < grid.Width; x++)
            {
                var point = new GridPoint(grid.Width - x - 1, row);
                var heightHere = grid.Get(point);
                if (heightHere > maxSoFar)
                {
                    visible.Set(point, true);
                    maxSoFar = heightHere;
                }
            }
        }

        for (var col = 0; col < grid.Width; col++)
        {
            var maxSoFar = -1;
            for (var y = 0; y < grid.Height; y++)
            {
                var point = new GridPoint(col, grid.Height - y - 1);
                var heightHere = grid.Get(point);
                if (heightHere > maxSoFar)
                {
                    visible.Set(point, true);
                    maxSoFar = heightHere;
                }
            }
        }

        for (var col = 0; col < grid.Width; col++)
        {
            var maxSoFar = -1;
            for (var y = 0; y < grid.Height; y++)
            {
                var point = new GridPoint(col, y);
                var heightHere = grid.Get(point);
                if (heightHere > maxSoFar)
                {
                    visible.Set(point, true);
                    maxSoFar = heightHere;
                }
            }
        }

        return visible.Trace().AllCells().Count(x => x);
    }


    public override object PartTwo(string[] input)
    {
        var grid = input.ToGrid().Map(x => int.Parse(x.ToString()));
        var region = grid.Region();
        var sightLines = region.AsCoordinateGrid.Map(p =>
        {
            var ourHeight = grid.Get(p);
            return new[] { GridDirection.Down, GridDirection.Up, GridDirection.Left, GridDirection.Right }
                .Select(direction =>
                {
                    var march = March(p, direction, region).ToList();
                    return march.FirstIndexOrDefault(tree => grid.Get(tree) >= ourHeight, march.Count - 1) + 1;
                })
                .Product();
        });

        return sightLines.AllCells().Max();
    }

    private IEnumerable<GridPoint> March(GridPoint initial, GridDirection direction, GridRegion region)
    {
        var current = initial;
        while (true)
        {
            current = current.Add(direction.AsUnitPoint(GridInterpretation.Graphics));
            if (region.ContainsPoint(current))
            {
                yield return current;
            }
            else
            {
                break;
            }
        }
    }
}