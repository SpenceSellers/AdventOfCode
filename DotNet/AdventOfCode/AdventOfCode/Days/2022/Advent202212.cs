using System;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.AStar;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2022;

public class Advent202212 : Problem
{
    public override object PartOne(string[] input)
    {
        var grid = input.ToGrid().Map(c => c switch
        {
            'S' => 0,
            'E' => 27,
            _ => c - 'a' + 1
        });

        var start = grid.FindPosition(c => c == 0);
        var end = grid.FindPosition(c => c == 27);

        var search = new AStarSearch<GridPoint>
        {
            Start = start,
            IsGoal = g => g == end,
            // The heuristic ended up not mattering. 
            Heuristic = gp => (gp - end).ManhattanDistanceFromOrigin(),
            // Heuristic = gp => 0,
            Neighbors = gp => gp.Adjacent4.Where(neighbor =>
            {
                if (!grid.Region().ContainsPoint(neighbor))
                {
                    return false;
                }

                if (grid.Get(neighbor) - grid.Get(gp) <= 1)
                {
                    return true;
                }

                return false;
            }).Select(x => (x, 1L)),
        };

        var results = search.Search();
        return results.Count - 1;
    }

    public override object PartTwo(string[] input)
    {
        var grid = input.ToGrid().Map(c => c switch
        {
            'S' => 0,
            'E' => 27,
            _ => c - 'a' + 1
        });
        var end = grid.FindPosition(c => c == 27);

        var allStarts = grid.AllEntries().Where(e => e.CellValue is 0 or 1).Select(x => x.Point).ToList();

        var i = 0;
        // I know how do to this the right way. I don't care.
        var distances = allStarts.AsParallel().Select(start =>
        {
            var search = new AStarSearch<GridPoint>
            {
                Start = start,
                IsGoal = g => g == end,
                Heuristic = gp => (gp - end).ManhattanDistanceFromOrigin(),
                // Heuristic = gp => 0,
                Neighbors = gp => gp.Adjacent4.Where(neighbor =>
                {
                    if (!grid.Region().ContainsPoint(neighbor))
                    {
                        return false;
                    }

                    if (grid.Get(neighbor) - grid.Get(gp) <= 1)
                    {
                        return true;
                    }

                    return false;
                }).Select(x => (x, 1L)),
            };

            i++;
            try
            {
                var results = search.Search();
                return results.Count - 1;
            }
            catch (UnreachableGoalException)
            {
                return int.MaxValue;
            }
        }).ToList();

        return distances.Min();
    }
}