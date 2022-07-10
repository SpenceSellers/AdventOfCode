using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Intcode;

namespace AdventOfCode.Days._2019;

public class Advent201905 : Problem
{
    public override object PartOne(string[] input)
    {
        var outputs = new List<int>();
        var computer = new IntcodeComputer(input.First().Split(",").Select(x => int.Parse(x)))
        {
            InputHandler = () => 1,
            OutputHandler = (x) =>
            {
                Console.Out.WriteLine(x);
                outputs.Add(x);
            }
        };
        computer.RunToCompletion();
        return outputs.Last();
    }

    public override object PartTwo(string[] input)
    {
        var outputs = new List<int>();
        var computer = new IntcodeComputer(input.First().Split(",").Select(x => int.Parse(x)))
        {
            InputHandler = () => 5,
            OutputHandler = (x) =>
            {
                Console.Out.WriteLine(x);
                outputs.Add(x);
            }
        };
        computer.RunToCompletion();
        return outputs.Last();
    }
}