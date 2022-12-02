using System;
using System.Linq;

namespace AdventOfCode.Days._2022;

public class Advent202202 : Problem
{
    public override object PartOne(string[] input)
    {
        var items = input.Select(line => line.Split(" ").Select(piece => Parse(piece[0])).ToList()).ToList();

        var score = 0;
        foreach (var item in items)
        {
            var theirs = item[0];
            var yours = item[1];
            var outcome = Beats(yours, theirs);

            score += ScoreSingle(yours, outcome);
        }

        return score;

    }

    private enum RockPaperScissor
    {
        Rock,
        Paper,
        Scissors
    }

    private RockPaperScissor Parse(char c)
    {
        return c switch
        {
            'A' or 'X' => RockPaperScissor.Rock,
            'B' or 'Y' => RockPaperScissor.Paper,
            'C' or 'Z' => RockPaperScissor.Scissors,
            _ => throw new ArgumentException("Invalid RockPaperScissors type")
        };
    }

    private int ScoreSingle(RockPaperScissor move, Outcome outcome)
    {
        var moveScore = move switch
        {
            RockPaperScissor.Rock => 1,
            RockPaperScissor.Paper => 2,
            RockPaperScissor.Scissors => 3
        };
        var outcomeScore = outcome switch
        {
            Outcome.Loses => 0,
            Outcome.Draw => 3,
            Outcome.Beats => 6
        };
        return moveScore + outcomeScore;
    }

    private enum Outcome
    {
        Beats,
        Loses,
        Draw
    }

    private Outcome Beats(RockPaperScissor a, RockPaperScissor b)
    {
        return (a, b) switch
        {
            (RockPaperScissor.Rock, RockPaperScissor.Paper) => Outcome.Loses,
            (RockPaperScissor.Rock, RockPaperScissor.Rock) => Outcome.Draw,
            (RockPaperScissor.Rock, RockPaperScissor.Scissors) => Outcome.Beats,
            (RockPaperScissor.Paper, RockPaperScissor.Rock) => Outcome.Beats,
            (RockPaperScissor.Paper, RockPaperScissor.Paper) => Outcome.Draw,
            (RockPaperScissor.Paper, RockPaperScissor.Scissors) => Outcome.Loses,
            (RockPaperScissor.Scissors, RockPaperScissor.Rock) => Outcome.Loses,
            (RockPaperScissor.Scissors, RockPaperScissor.Paper) => Outcome.Beats,
            (RockPaperScissor.Scissors, RockPaperScissor.Scissors) => Outcome.Draw,
        };
    }

    public override object PartTwo(string[] input)
    {
        throw new System.NotImplementedException();
    }
}