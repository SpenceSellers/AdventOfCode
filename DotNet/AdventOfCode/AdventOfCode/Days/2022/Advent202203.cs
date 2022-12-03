using System.Diagnostics;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2022;

public class Advent202203 : Problem
{
    public override object PartOne(string[] input)
    {
        return input.Select(x =>
        {
            var size = x.Length / 2;
            var first = x[..size].ToHashSet();
            var second = x[size..].ToHashSet();
            first.IntersectWith(second);
            return first.Select(Score).Sum();
        }).Sum();
    }

    public override object PartTwo(string[] input)
    {
        return input.Chunks(3).Select(rows =>
        {
            var letters = rows[0].ToHashSet();
            letters.IntersectWith(rows[1]);
            letters.IntersectWith(rows[2]);
            return letters.Select(Score).Sum();
        }).Sum();
    }

    private int Score(char c)
    {
        var alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        Debug.Assert(alphabet.Length == 26 * 2); // I 100% forgot W existed the first time around...
        return alphabet.IndexOf(c) + 1;
    }
}