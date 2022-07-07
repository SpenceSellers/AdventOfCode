using System.Linq;
using System.Text.Json;
using AdventOfCode.AdventLib.Intcode;

namespace AdventOfCode.Days._2019;

public class Advent201902 : Problem
{
    public override object PartOne(string[] input)
    {
        var computer = new IntcodeComputer(input.First().Split(",").Select(x => int.Parse(x)));
        computer.Nums[1] = 12;
        computer.Nums[2] = 2;
        computer.RunToCompletion();
        return JsonSerializer.Serialize(computer.Nums);
    }

    public override object PartTwo(string[] input)
    {
        for (int i = 0; i < 100; i++)
        {
            for (int j = 0; j < 100; j++)
            {
                var computer = new IntcodeComputer(input.First().Split(",").Select(x => int.Parse(x)));
                computer.Nums[1] = i;
                computer.Nums[2] = j;
                computer.RunToCompletion();
                if (computer.Nums[0] == 19690720)
                {
                    return i * 100 + j;
                }
            }
        }

        return null;
    }
}