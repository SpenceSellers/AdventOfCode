using System;
using System.Collections.Generic;
using System.Linq;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;
using AdventOfCode.AdventLib.Parsing;

namespace AdventOfCode.Days._2021
{
    public class Advent202104 : Problem
    {
        public override object PartOne(string[] input)
        {
            var balls = input[0]
                .Split(",")
                .Select(int.Parse)
                .ToList();
            var boards = ParseBoards(input);

            var (winningRound, winningBoard) = BallRounds(balls)
                .SelectAlongside(round => boards.FirstOrDefault(board => BoardHasWon(board, round.ToHashSet())))
                .FirstOrDefault(a => a.Item2 != null);

            var winningBall = winningRound.Last();
            return winningBoard
                .AllCells()
                .Except(winningRound)
                .Sum() * winningBall;
        }

        public override object PartTwo(string[] input)
        {
            var balls = input[0]
                .Split(",")
                .Select(int.Parse)
                .ToList();
            var boards = ParseBoards(input);

            foreach (var ballsForThisRound in BallRounds(balls))
            {
                var boardsThatWonThisRound = boards
                    .Where(board => BoardHasWon(board, ballsForThisRound.ToHashSet()))
                    .ToList();

                // Was this the last winning board?
                if (boards.Count == 1 & boardsThatWonThisRound.Count == 1)
                {
                    var winningBoard = boardsThatWonThisRound.Single();
                    var winningBall = ballsForThisRound.Last();
                    var boardSum = winningBoard
                        .AllCells()
                        .Except(ballsForThisRound)
                        .Sum();
                    return boardSum * winningBall;
                }
                foreach (var winningBoard in boardsThatWonThisRound)
                {
                    boards.Remove(winningBoard);
                }
            }

            return null;
        }

        /// <summary>
        /// Bingo is theoretically a stateless game. Return each successive state of the game: First the first ball,
        /// then the first two balls, then the first three, etc.
        /// </summary>
        private IEnumerable<List<int>> BallRounds(List<int> balls)
        {
            return balls.Select((_, i) => balls.Take(i).ToList());
        }

        // I literally added .Rows() and .Columns() to my grid library right before I saw the problem.
        private bool BoardHasWon(IDefinedSizeGrid<int> board, HashSet<int> balls) =>
            board.Columns().Any(col => col.All(balls.Contains)) ||
            board.Rows().Any(row => row.All(balls.Contains));

        private static List<SolidGrid<int>> ParseBoards(string[] input)
        {
            return new SeparatedGroupParser()
                .Parse(input[2..])
                .Select(boardGroup => boardGroup
                    .Select(boardLine => boardLine.Split(" ", StringSplitOptions.RemoveEmptyEntries).Select(int.Parse)))
                .Select(board => new SolidGrid<int>(board))
                .Realize();
        }
    }
}