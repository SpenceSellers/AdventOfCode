using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2022;

public class Advent202208 : Problem
{
    private readonly GridDirection[] _allGridDirections = { GridDirection.Down, GridDirection.Up, GridDirection.Left, GridDirection.Right };

    public override object PartOne(string[] input)
    {
        var grid = input.ToGrid().Map(x => int.Parse(x.ToString()));
        var region = grid.Region();
        return region.AsCoordinateGrid.Map(p =>
            {
                var ourHeight = grid.Get(p);
                return _allGridDirections.Any(direction => March(p,
                        direction,
                        region)
                    .All(marchPoint => grid.Get(marchPoint) < ourHeight));
            })
            .AllCells()
            .Count(x => x);
    }

    public override object PartTwo(string[] input)
    {
        var grid = input.ToGrid().Map(x => int.Parse(x.ToString()));
        var region = grid.Region();
        var sightLines = region.AsCoordinateGrid.Map(p =>
        {
            var ourHeight = grid.Get(p);
            return _allGridDirections
                .Select(direction =>
                {
                    var march = March(p, direction, region).ToList();
                    return march.FirstIndexOrDefault(tree => grid.Get(tree) >= ourHeight, march.Count - 1) + 1;
                })
                .Product();
        });

        return sightLines.AllCells()
            .Max();
    }

    /// <summary>
    /// Yield points successively further and further in a direction until we leave the defined region
    /// </summary>
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