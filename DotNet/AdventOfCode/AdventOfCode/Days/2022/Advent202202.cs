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
                var (theirMove, yourMove) = x;
                var outcome = Beats(yourMove, theirMove);

                return ScoreSingle(yourMove, outcome);
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
                var theirMove = ParseMove(theirsChar);
                var yourMove = MoveToCreateOutcome(theirMove, ParseOutcome(yoursChar));

                return ScoreSingle(yourMove, ParseOutcome(yoursChar));
            })
            .Sum();
    }

    private enum Move
    {
        Rock,
        Paper,
        Scissors
    }

    private enum Outcome
    {
        Beats,
        Loses,
        Draw
    }

    private Move ParseMove(char c) =>
        c switch
        {
            'A' or 'X' => Move.Rock,
            'B' or 'Y' => Move.Paper,
            'C' or 'Z' => Move.Scissors,
            _ => throw new ArgumentException("Invalid RockPaperScissors type")
        };

    private Outcome ParseOutcome(char c) =>
        c switch
        {
            'X' => Outcome.Loses,
            'Y' => Outcome.Draw,
            'Z' => Outcome.Beats
        };

    private int ScoreSingle(Move move, Outcome outcome)
    {
        var moveScore = move switch
        {
            Move.Rock => 1,
            Move.Paper => 2,
            Move.Scissors => 3
        };
        var outcomeScore = outcome switch
        {
            Outcome.Loses => 0,
            Outcome.Draw => 3,
            Outcome.Beats => 6
        };
        return moveScore + outcomeScore;
    }

    private Outcome Beats(Move a, Move b) =>
        (a, b) switch
        {
            (Move.Rock, Move.Scissors) => Outcome.Beats,
            (Move.Paper, Move.Rock) => Outcome.Beats,
            (Move.Scissors, Move.Paper) => Outcome.Beats,
            (_, _) when a == b => Outcome.Draw,
            _ => Outcome.Loses
        };

    private Move MoveToCreateOutcome(Move theirs, Outcome outcome) =>
        (theirs, outcome) switch
        {
            (_, Outcome.Draw) => theirs,
            (Move.Paper, Outcome.Beats) => Move.Scissors,
            (Move.Paper, Outcome.Loses) => Move.Rock,
            (Move.Rock, Outcome.Beats) => Move.Paper,
            (Move.Rock, Outcome.Loses) => Move.Scissors,
            (Move.Scissors, Outcome.Beats) => Move.Rock,
            (Move.Scissors, Outcome.Loses) => Move.Paper,
        };
}