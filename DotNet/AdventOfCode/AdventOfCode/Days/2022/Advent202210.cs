using System;
using System.Diagnostics;
using System.Linq;

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
        throw new System.NotImplementedException();
    }
}