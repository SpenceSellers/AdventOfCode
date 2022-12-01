using System.Linq;
using AdventOfCode.AdventLib.Parsing;

namespace AdventOfCode.Days._2022;

public class Advent202201 : Problem
{
    public override object PartOne(string[] input)
    {
        var groups = new SeparatedGroupParser().Parse(input);
        var intGroups = groups.Select(group => group.Select(int.Parse));
        return intGroups.MaxBy(g => g.Sum()).Sum();
    }

    public override object PartTwo(string[] input)
    {
        var groups = new SeparatedGroupParser().Parse(input);
        var intGroups = groups.Select(group => group.Select(int.Parse));
        return intGroups.OrderByDescending(x => x.Sum()).Take(3).Select(x => x.Sum()).Sum();
    }
}