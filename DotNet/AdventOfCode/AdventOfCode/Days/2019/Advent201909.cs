using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib.Intcode;

namespace AdventOfCode.Days._2019;

public class Advent201909 : Problem
{
    public override object PartOne(string[] input)
    {
        var outputs = new List<long>();
        var computer = new IntcodeComputer(input.First().Split(",").Select(x => long.Parse(x)))
        {
            InputHandler = () => 1,
            OutputHandler = (x) =>
            {
                Console.Out.WriteLine("Got " + x);
                outputs.Add(x);
            }
        };
        computer.RunToCompletion();
        return outputs.Last();
    }

    public override object PartTwo(string[] input)
    {
        throw new System.NotImplementedException();
    }
}