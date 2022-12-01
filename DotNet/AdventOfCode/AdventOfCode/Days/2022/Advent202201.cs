using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib.Parsing;

namespace AdventOfCode.Days._2022;

public class Advent202201 : Problem
{
    public override object PartOne(string[] input)
    {
        return new SeparatedGroupParser()
            .Parse(input)
            .Select(group => group.Select(int.Parse).Sum())
            .Max();
    }

    public override object PartTwo(string[] input)
    {
        return new SeparatedGroupParser()
            .Parse(input)
            .Select(group => group.Select(int.Parse).Sum())
            .OrderDescending()
            .Take(3)
            .Sum();
    }
}