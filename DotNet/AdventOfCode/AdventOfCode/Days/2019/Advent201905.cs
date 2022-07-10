using System.Linq;
using AdventOfCode.AdventLib.Intcode;

namespace AdventOfCode.Days._2019;

public class Advent201905 : Problem
{
    public override object PartOne(string[] input)
    {
        var computer = new IntcodeComputer(input.First().Split(",").Select(x => int.Parse(x)));
        return null;
    }

    public override object PartTwo(string[] input)
    {
        var computer = new IntcodeComputer(input.First().Split(",").Select(x => int.Parse(x)));
        return null;
    }
}