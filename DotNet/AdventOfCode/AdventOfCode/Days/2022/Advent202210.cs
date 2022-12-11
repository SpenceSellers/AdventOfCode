using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2022;

public class Advent202210 : Problem
{
    public override object PartOne(string[] input)
    {
        var x = 1;
        var cycle = 0;
        var score = 0;
        foreach (var line in input)
        {
            var (cycleSpan, toAdd) = ParseLine(line);

            for (int i = 0; i < cycleSpan; i++)
            {
                // Why do we have to count BACKWARDS here? Help
                var sampledCycle = cycle + cycleSpan - i;
                if (ShouldCaptureCycle(sampledCycle))
                {
                    score += sampledCycle * x;
                }
            }

            x += toAdd;
            cycle += cycleSpan;
        }

        return score;
    }

    private bool ShouldCaptureCycle(int cycle)
    {
        return (cycle - 20) % 40 == 0;
    }

    public override object PartTwo(string[] input)
    {
        var x = 1;
        var cycle = 0;
        var score = 0;
        var display = new SolidGrid<bool>(40, 6, false);
        foreach (var line in input)
        {
            var (cycleSpan, toAdd) = ParseLine(line);

            for (int i = 0; i < cycleSpan; i++)
            {
                var sampledCycle = cycle + i;
                var cyclePos = sampledCycle % 40;
                var row = sampledCycle / 40;

                if (Math.Abs(cyclePos - x ) <= 1)
                {
                    display.Set(new GridPoint(cyclePos, row), true);
                }
            }

            x += toAdd;
            cycle += cycleSpan;
        }

        display.Trace();
        return null;
    }

    private static (int cycleSpan, int toAdd) ParseLine(string line)
    {
        var (cycleSpan, toAdd) = line switch
        {
            "noop" => (1, 0),
            _ => (2, int.Parse(line.Split()[1]))
        };
        return (cycleSpan, toAdd);
    }
}