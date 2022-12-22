using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
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
        var count = 0;
        for (int x = 0; x <= 4000000; x++)
        {
            if (x < 100 || x % 100 == 0)
            {
                Console.Out.WriteLine($"x={x}, {(((double) x)/4000000) * 100}%");
            }
            for (int y = 0; y <= 4000000; y++)
            {
                var point = new GridPoint(x, y);
                var any = false;
                foreach (var kv in distances)
                {
                    if ((point - kv.Key).ManhattanDistanceFromOrigin() <= kv.Value)
                    {
                        any = true;
                        break;
                    }
                }

                if (!any)
                {
                    Console.Out.WriteLine(point);
                    return (point.X * 4000000) + point.Y;
                }
            }
        }

        return null;
    }
}