using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
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

    public override object PartTwo(string[] input)
    {
        var monkeyChunks = input.SplitList("");
        var monkeys = monkeyChunks.Select(ParseMonkey).ToArray();
        var monkeyGame = new MonkeyGame2(monkeys);
        for (int i = 0; i < 10000; i++)
        {
            monkeyGame.PlayRound();
        }

        var (i0, i1) = monkeys.Select(m => m.InspectionCount).OrderDescending().Take(2).Select(i => new BigInteger(i)).Two();
        return i0 * i1;
    }

    private class Monkey
    {
        public DoubleEndedList<long> Items = new();
        public Func<long, long> Function;
        public int IfTrueMonkey;
        public int IfFalseMonkey;
        public long DivisibleBy;

        public int InspectionCount;
    }

    private class MonkeyGame2
    {
        public readonly IList<Monkey> Monkeys;
        private readonly long _aggregateNumber;

        public MonkeyGame2(IList<Monkey> monkeys)
        {
            Monkeys = monkeys;
            _aggregateNumber = monkeys.Select(x => x.DivisibleBy).Product();
            Console.Out.WriteLine($"Aggregate number is {_aggregateNumber}");
        }

        public void PlayMonkey(int monkeyId)
        {
            var monkey = Monkeys[monkeyId];
            while (monkey.Items.Count != 0)
            {
                monkey.InspectionCount++;
                var item = monkey.Items.PopFront();
                var newValue = monkey.Function(item) % _aggregateNumber;
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
                .Select(long.Parse)
                .ToDoubleEndedList(),
            Function = ParseOperation(lines[2].Split("=", StringSplitOptions.TrimEntries)[1]),
            DivisibleBy = long.Parse(lines[3].Split().Last()),
            IfTrueMonkey = int.Parse(lines[4].Split().Last()),
            IfFalseMonkey = int.Parse(lines[5].Split().Last())
        };
    }

    private Func<long, long> ParseOperation(string operation)
    {
        var pieces = operation.Split();

        long VarA(long i) =>
            pieces[0] switch
            {
                "old" => i,
                _ => long.Parse(pieces[0])
            };

        long VarB(long i) =>
            pieces[2] switch
            {
                "old" => i,
                _ => long.Parse(pieces[2])
            };

        return pieces[1] switch
        {
            "+" => i => VarA(i) + VarB(i),
            "*" => i => VarA(i) * VarB(i)
        };
    }
}