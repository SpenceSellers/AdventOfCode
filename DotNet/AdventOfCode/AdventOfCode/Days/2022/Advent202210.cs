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
            int cycleSpan;
            var toAdd = 0;
            if (line == "noop")
            {
                cycleSpan = 1;
            }
            else
            {
                toAdd = int.Parse(line.Split()[1]);
                cycleSpan = 2;
            }

            for (int i = 0; i < cycleSpan; i++)
            {
                var sampledCycle = cycle + cycleSpan - i;
                if (ShouldCaptureCycle(sampledCycle))
                {
                    Console.Out.WriteLine($"Capturing sampled cycle {sampledCycle}, cycle={cycle}, x = {x}, score adding {sampledCycle * x }");
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
        var display = new SolidGrid<bool>(40, 10, false);
        foreach (var line in input)
        {
            int cycleSpan;
            var toAdd = 0;
            if (line == "noop")
            {
                cycleSpan = 1;
            }
            else
            {
                toAdd = int.Parse(line.Split()[1]);
                cycleSpan = 2;
            }

            for (int i = 0; i < cycleSpan; i++)
            {
                var sampledCycle = cycle + cycleSpan - i;
                var cyclePos = sampledCycle % 40;
                var row = sampledCycle / 40;

                // Why minus one? I have no idea. Is it because cycles are supposed to be 1-indexed?
                // Changing the first cycle to one doesn't help at all.
                if (Math.Abs(cyclePos - x - 1 ) <= 1)
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
}