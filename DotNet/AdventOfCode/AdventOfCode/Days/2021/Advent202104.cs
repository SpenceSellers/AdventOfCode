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
            var balls = input[0].Split(",").Select(int.Parse).ToList();
            var boardLines = new SeparatedGroupParser().Parse(input[2..]);
            var boards = boardLines
                .Select(boardGroup => boardGroup
                    .Select(boardLine => boardLine.Split(" ", StringSplitOptions.RemoveEmptyEntries).Select(int.Parse)))
                .Select(board => new SolidGrid<int>(board))
                .Realize();

            for (int i = 0; i < balls.Count; i++)
            {
                var ballsForThisRound = balls.Take(i);
                var winningBoard = boards.FirstOrDefault(board => BoardHasWon(board, ballsForThisRound.ToHashSet()));
                if (winningBoard is not null)
                {
                    var winningNumber = ballsForThisRound.Last();
                    var nums = winningBoard.AllCells().ToHashSet().Except(ballsForThisRound).Sum();
                    return nums * winningNumber;
                }
            }

            return null;
        }

        private bool BoardHasWon(IDefinedSizeGrid<int> board, HashSet<int> balls)
        {
            if (board.Columns().Any(col => col.All(balls.Contains)))
            {
                return true;
            }

            if (board.Rows().Any(row => row.All(balls.Contains)))
            {
                return true;
            }

            return false;
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}