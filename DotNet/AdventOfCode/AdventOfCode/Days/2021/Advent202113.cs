using System;
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

            var points = coordLines
                .Select(line => line.Split(",").Select(int.Parse).Two())
                .Select(i => new GridPoint(i.Item1, i.Item2))
                .ToGrid();

            var folds = foldLines
                .Select(line => new Regex("fold along (.)=(.*)").Captures(line).Two())
                .Select(a => new Fold(a.Item1 == "x" ? Axis.X : Axis.Y, int.Parse(a.Item2)))
                .ToList();

            var folded = DoFold(points, folds[0]);

            Console.Out.WriteLine(folded.AsDefinedSizeNotPreservingCoordinates().Dump());

            return folded.AllDefinedCells.Count();
        }

        private SparseGrid<bool> DoFold(SparseGrid<bool> grid, Fold fold)
        {
            var nextGrid = new SparseGrid<bool>();
            var region = grid.BoundingRegion();
            var smaller = fold.Axis == Axis.X ? region.Origin.X : region.Origin.Y;
            var larger = fold.Axis == Axis.X ? region.Origin.X + region.Width : region.Origin.Y + region.Height;
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
                        Console.Out.WriteLine($"After fold is {gridPoint}");
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
                        Console.Out.WriteLine($"{point} After fold {fold} is {gridPoint}");
                        nextGrid.Set(gridPoint, true);
                    }
                }
            }

            return nextGrid;
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}