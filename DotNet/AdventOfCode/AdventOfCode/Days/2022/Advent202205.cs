using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.DataStructures;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2022;

public class Advent202205 : Problem
{
    public override object PartOne(string[] input)
    {
        var (state, moves) = ParseProblem(input);

        foreach (var (count, from, to) in moves)
        {
            for (int i = 0; i < count; i++)
            {
                var item = state[from].PopBack();
                state[to].PushBack(item);
            }
        }

        return string.Join("", state.Select(stack => stack.Last()));
    }

    public override object PartTwo(string[] input)
    {
        var (state, moves) = ParseProblem(input);

        foreach (var (count, from, to) in moves)
        {
            var toMove = new DoubleEndedList<char>();
            for (int i = 0; i < count; i++)
            {
                var item = state[from].PopBack();
                toMove.PushFront(item);
            }
            state[to].AddRangeBack(toMove);
        }

        return string.Join("", state.Select(stack => stack.Last()));
    }

    private record Move(int Count, int From, int To);

    private (List<DoubleEndedList<char>> state, Move[] moves) ParseProblem(IEnumerable<string> input)
    {
        var (firstBlock, secondBlock) = input.SplitList("").Two();
        var state = ParseInitialState(firstBlock);

        var regex = new Regex(@"^move (.*) from (.*) to (.*)$");

        var moves = secondBlock.Select(line =>
        {
            var captures = regex.Captures(line).Select(int.Parse).ToArray();
            return new Move(captures[0], captures[1] - 1, captures[2] - 1);
        }).ToArray();
        return (state, moves);
    }

    private List<DoubleEndedList<char>> ParseInitialState(IEnumerable<string> lines)
    {
        return lines.ToGrid()
            .RotateClockwise()
            .Rows()
            .Where(row => row.First() != ' ')
            .Select(row => row.Skip(1)
                .Where(c => c != ' ')
                .ToDoubleEndedList())
            .ToList();
    }
}