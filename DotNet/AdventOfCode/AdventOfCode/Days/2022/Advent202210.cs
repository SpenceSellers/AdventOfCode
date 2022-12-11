using System;
using AdventOfCode.AdventLib.Grid;
using AdventOfCode.AdventLib.Grid.SparseGrids;

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

            // Loop through all the cycles that this instruction is going to consume
            for (int i = 0; i < cycleSpan; i++)
            {
                // Why plus one? Part 2 works without this.
                var sampledCycle = cycle + i + 1;
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
        var display = new SparseGrid<bool>();
        foreach (var line in input)
        {
            var (cycleSpan, toAdd) = ParseLine(line);

            // Loop through all the cycles that this instruction is going to consume
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

        display.AsDefinedSizeNotPreservingCoordinates().Trace();
        return null;
    }

    private static (int cycleSpan, int toAdd) ParseLine(string line)
    {
        return line switch
        {
            "noop" => (1, 0),
            _ => (2, int.Parse(line.Split()[1]))
        };
    }
}