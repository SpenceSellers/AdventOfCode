using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;
using AdventOfCode.AdventLib.Parsing;

namespace AdventOfCode.Days._2021
{
    public class Advent202113 : Problem
    {
        private enum Axis
        {
            X, Y
        }

        private record Fold(Axis Axis, int Coordinate);
        public override object PartOne(string[] input)
        {
            var (coordLines, foldLines) = new SeparatedGroupParser().Parse(input).Two();

            var points = ParseCoordinates(coordLines);

            var folds = ParseFolds(foldLines);

            var folded = DoFold(points, folds[0]);

            Console.Out.WriteLine(folded.AsDefinedSizeNotPreservingCoordinates().Dump());

            return folded.AllDefinedCells.Count();
        }

        public override object PartTwo(string[] input)
        {
            var (coordLines, foldLines) = new SeparatedGroupParser().Parse(input).Two();

            var points = ParseCoordinates(coordLines);

            var folds = ParseFolds(foldLines);

            // var folded = DoFold(points, folds[0]);
            var folded = points;
            foreach (var fold in folds)
            {
                folded = DoFold(folded, fold);
            }

            return folded.AsDefinedSizeNotPreservingCoordinates().Dump();
        }

        private static List<Fold> ParseFolds(IEnumerable<string> foldLines)
        {
            var folds = foldLines
                .Select(line => new Regex("fold along (.)=(.*)").Captures(line).Two())
                .Select(a => new Fold(a.Item1 == "x" ? Axis.X : Axis.Y, int.Parse(a.Item2)))
                .ToList();
            return folds;
        }

        private static SparseGrid<bool> ParseCoordinates(IEnumerable<string> coordLines)
        {
            var points = coordLines
                .Select(line => line.Split(",").Select(int.Parse).Two())
                .Select(i => new GridPoint(i.Item1, i.Item2))
                .ToGrid();
            return points;
        }

        private SparseGrid<bool> DoFold(SparseGrid<bool> grid, Fold fold)
        {
            var nextGrid = new SparseGrid<bool>();
            foreach (var (point, _) in grid.AllDefinedCells)
            {
                if (fold.Axis == Axis.X)
                {
                    if (point.X < fold.Coordinate)
                    {
                        nextGrid.Set(point, true);
                    }
                    else if (point.X == fold.Coordinate)
                    {
                        // Do nothing
                    }
                    else
                    {
                        var gridPoint = new GridPoint(fold.Coordinate - Math.Abs(fold.Coordinate - point.X), point.Y);
                        nextGrid.Set(gridPoint, true);
                    }
                }
                else
                {
                    if (point.Y < fold.Coordinate)
                    {
                        nextGrid.Set(point, true);
                    }
                    else if (point.Y == fold.Coordinate)
                    {
                        // Do nothing
                    }
                    else
                    {
                        var gridPoint = new GridPoint(point.X, fold.Coordinate - Math.Abs(fold.Coordinate - point.Y));
                        nextGrid.Set(gridPoint, true);
                    }
                }
            }

            return nextGrid;
        }
    }
}