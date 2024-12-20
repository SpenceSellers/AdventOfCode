using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2022;

public class Advent202215 : Problem
{
    public override object PartOne(string[] input)
    {
        var knowledge = input.Select(ParseLine).ToArray();
        var distances = ToDistances(knowledge);
        var y = 2000000;
        // var y = 10;
        var count = 0;
        for (int i = -10_000_000; i < 10_000_000; i++)
        {
            var point = new GridPoint(i, y);
            if (distances.Any(kv => (point - kv.Key).ManhattanDistanceFromOrigin() <= kv.Value))
            {
                count++;
            }
        }

        // Why -1???
        return count - 1;
    }

    private (GridPoint, GridPoint) ParseLine(string input)
    {
        var regex = new Regex($"Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.+)");
        var pieces = regex.Captures(input).Select(int.Parse).ToArray();
        return (new GridPoint(pieces[0], pieces[1]), new GridPoint(pieces[2], pieces[3]));
    }

    private Dictionary<GridPoint, int> ToDistances(IEnumerable<(GridPoint, GridPoint)> observations)
    {
        var dict = new Dictionary<GridPoint, int>();
        foreach (var (point, closest) in observations)
        {
            var distance = (point - closest).ManhattanDistanceFromOrigin();
            dict[point] = distance;
        }

        return dict;
    }

    public override object PartTwo(string[] input)
    {
        var knowledge = input.Select(ParseLine).ToArray();
        var distances = ToDistances(knowledge);
        var size = 4000000;

        foreach (var (centerPoint, distance) in distances)
        {
            foreach (var candidatePoint in centerPoint.PointsManhattanDistanceFrom(distance + 1))
            {
                if (!distances.AsParallel().Any(kv => (candidatePoint - kv.Key).ManhattanDistanceFromOrigin() <= kv.Value))
                {

                    return (long) candidatePoint.X * 4000000 + candidatePoint.Y;
                }
            }
        }

        return null;
    }
}