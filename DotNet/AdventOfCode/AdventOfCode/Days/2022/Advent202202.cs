using System;
using System.Linq;

namespace AdventOfCode.Days._2022;

public class Advent202202 : Problem
{
    public override object PartOne(string[] input)
    {
        return input
            .Select(line => line.Split(" ").Select(piece => ParseMove(piece[0])).Two())
            .Select(x =>
            {
                var (theirs, yours) = x;
                var outcome = Beats(yours, theirs);

                return ScoreSingle(yours, outcome);
            })
            .Sum();
    }

    public override object PartTwo(string[] input)
    {
        return input
            .Select(line => line.Split(" ").Select(piece => piece[0]).Two())
            .Select(x =>
            {
                var (theirsChar, yoursChar) = x;
                var theirs = ParseMove(theirsChar);
                var desiredOutcome = ParseOutcome(yoursChar);
                var yourMove = MoveToCreateOutcome(theirs, desiredOutcome);

                return ScoreSingle(yourMove, desiredOutcome);
            })
            .Sum();
    }

    private enum RockPaperScissor
    {
        Rock,
        Paper,
        Scissors
    }

    private RockPaperScissor ParseMove(char c) =>
        c switch
        {
            'A' or 'X' => RockPaperScissor.Rock,
            'B' or 'Y' => RockPaperScissor.Paper,
            'C' or 'Z' => RockPaperScissor.Scissors,
            _ => throw new ArgumentException("Invalid RockPaperScissors type")
        };

    private Outcome ParseOutcome(char c) =>
        c switch
        {
            'X' => Outcome.Loses,
            'Y' => Outcome.Draw,
            'Z' => Outcome.Beats
        };

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

    private Outcome Beats(RockPaperScissor a, RockPaperScissor b) =>
        (a, b) switch
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

    private RockPaperScissor MoveToCreateOutcome(RockPaperScissor theirs, Outcome outcome) =>
        (theirs, outcome) switch
        {
            (_, Outcome.Draw) => theirs,
            (RockPaperScissor.Paper, Outcome.Beats) => RockPaperScissor.Scissors,
            (RockPaperScissor.Paper, Outcome.Loses) => RockPaperScissor.Rock,
            (RockPaperScissor.Rock, Outcome.Beats) => RockPaperScissor.Paper,
            (RockPaperScissor.Rock, Outcome.Loses) => RockPaperScissor.Scissors,
            (RockPaperScissor.Scissors, Outcome.Beats) => RockPaperScissor.Rock,
            (RockPaperScissor.Scissors, Outcome.Loses) => RockPaperScissor.Paper,
        };
}