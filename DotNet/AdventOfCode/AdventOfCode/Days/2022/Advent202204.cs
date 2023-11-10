using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2022;

public class Advent202204 : Problem
{
    public override object PartOne(string[] input)
    {
        return input
            .Select(ParseLine)
            .Count(x => x.Item1.FullyContains(x.Item2) || x.Item2.FullyContains(x.Item1));
    }

    public override object PartTwo(string[] input)
    {
        return input
            .Select(ParseLine)
            .Count(x => x.Item1.OverlapsAtAll(x.Item2));
    }

    private (Assignment, Assignment) ParseLine(string line)
    {
        var ((s0, e0), (s1, e1)) = line
            .Split(",")
            .Select(x => x.Split("-").Select(int.Parse).Two())
            .Two();
        return (new Assignment(s0, e0), new Assignment(s1, e1));
    }

    private record Assignment(int Start, int End)
    {
        public bool FullyContains(Assignment other)
        {
            return other.Start >= Start && other.End <= End;
        }

        public bool OverlapsAtAll(Assignment other)
        {
            return other.Start <= End && other.End >= Start;
        }
    }
}