using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.DataStructures;

namespace AdventOfCode.Days._2022;

public class Advent202211 : Problem
{
    public override object PartOne(string[] input)
    {
        var monkeyChunks = input.SplitList("");
        var monkeys = monkeyChunks.Select(ParseMonkey).ToArray();
        var monkeyGame = new MonkeyGame(monkeys);
        for (int i = 0; i < 20; i++)
        {
            monkeyGame.PlayRound();
        }

        return monkeys.Select(m => m.InspectionCount).OrderDescending().Take(2).Product();
    }

    private class Monkey
    {
        public DoubleEndedList<int> Items = new();
        public Func<int, int> Function;
        public int IfTrueMonkey;
        public int IfFalseMonkey;
        public int DivisibleBy;

        public int InspectionCount;
    }

    private class MonkeyGame
    {
        public readonly IList<Monkey> Monkeys;

        public MonkeyGame(IList<Monkey> monkeys)
        {
            Monkeys = monkeys;
        }

        public void PlayMonkey(int monkeyId)
        {
            var monkey = Monkeys[monkeyId];
            while (monkey.Items.Count != 0)
            {
                monkey.InspectionCount++;
                var item = monkey.Items.PopFront();
                var newValue = monkey.Function(item) / 3;
                if (newValue % monkey.DivisibleBy == 0)
                {
                    Monkeys[monkey.IfTrueMonkey].Items.PushBack(newValue);
                }
                else
                {
                    Monkeys[monkey.IfFalseMonkey].Items.PushBack(newValue);
                }
            }
        }

        public void PlayRound()
        {
            for (int i = 0; i < Monkeys.Count; i++)
            {
                PlayMonkey(i);
            }
        }
    }

    private Monkey ParseMonkey(IList<string> lines)
    {
        return new Monkey
        {
            Items = lines[1].Split(":")[1]
                .Split(",", StringSplitOptions.TrimEntries)
                .Select(int.Parse)
                .ToDoubleEndedList(),
            Function = ParseOperation(lines[2].Split("=", StringSplitOptions.TrimEntries)[1]),
            DivisibleBy = int.Parse(lines[3].Split().Last()),
            IfTrueMonkey = int.Parse(lines[4].Split().Last()),
            IfFalseMonkey = int.Parse(lines[5].Split().Last())
        };
    }

    private Func<int, int> ParseOperation(string operation)
    {
        var pieces = operation.Split();

        int VarA(int i) =>
            pieces[0] switch
            {
                "old" => i,
                _ => int.Parse(pieces[0])
            };

        int VarB(int i) =>
            pieces[2] switch
            {
                "old" => i,
                _ => int.Parse(pieces[2])
            };

        return pieces[1] switch
        {
            "+" => i => VarA(i) + VarB(i),
            "*" => i => VarA(i) * VarB(i)
        };
    }

    public override object PartTwo(string[] input)
    {
        throw new System.NotImplementedException();
    }
}