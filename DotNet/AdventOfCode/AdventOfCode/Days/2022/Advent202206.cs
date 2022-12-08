using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2022;

public class Advent202206 : Problem
{
    public override object PartOne(string[] input) => FindFirstUnique(input.First(), 4);
    public override object PartTwo(string[] input) => FindFirstUnique(input.First(), 14);

    private int FindFirstUnique<T>(IEnumerable<T> items, int len) =>
        items.SequencesOfSize(len).FirstIndex(sequence => sequence.Distinct().Count() == len) + len;
}