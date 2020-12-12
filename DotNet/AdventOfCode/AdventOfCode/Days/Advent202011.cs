using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days
{
    public class Advent202011 : Problem
    {
        public Advent202011() : base(2020, 11)
        {
        }

        public override string PartOne(string[] input)
        {
            return SolvePart(input, StepPart1);
        }

        public override string PartTwo(string[] input)
        {
            return SolvePart(input, StepPart2);
        }

        private static string SolvePart(IEnumerable<string> input, Func<IDefinedSizeGrid<char>, IDefinedSizeGrid<char>> stepFunc)
        {
            var initial = SolidGrid<char>.Extract(input);
            var states = new List<IDefinedSizeGrid<char>> {initial};
            
            while (true)
            {
                var prevState = states.Last();
                var newState = stepFunc(prevState).Solidify();
                states.Add(newState);

                if (prevState.Overlay(newState, (c1, c2) => c1 == c2)
                    .Windowed(initial.Region())
                    .AllCells()
                    .All(b => b))
                {
                    break;
                };
            }

            return states.Last().AllCells().Count(c => c == '#').ToString();
        }

        private static IDefinedSizeGrid<char> StepPart1(IDefinedSizeGrid<char> chars)
        {
            return CommonGrids.CoordinateGrid
                .Map(coord => (chars.Get(coord), SurroundingEight(chars, coord, '.')))
                .Map(x => StepFunction(x.Item2, x.Item1, 4))
                .Windowed(chars.Region());
        }

        private static IDefinedSizeGrid<char> StepPart2(IDefinedSizeGrid<char> chars)
        {
            return CommonGrids.CoordinateGrid
                .Map(coord => (chars.Get(coord), SurroundingEightSightlines(chars, coord)))
                .Map(x => StepFunction(x.Item2, x.Item1, 5))
                .Windowed(chars.Region());
        }

        private static char StepFunction(IEnumerable<char> surround, char self, int threshold)
        {
            return self switch
            {
                '.' => '.',
                '#' => surround.Count(c => c == '#') >= threshold ? 'L' : '#',
                'L' => surround.Any(c => c == '#') ? 'L' : '#'
            };
        }

        private static IEnumerable<char> SurroundingEight(IDefinedSizeGrid<char> grid, GridPoint p, char backup)
        {
            foreach (var offset in EightSlopes())
            {
                var pos = p + offset;
                if (grid.Region().ContainsPoint(pos))
                {
                    yield return grid.Get(pos);
                }
                else
                {
                    yield return backup;
                }
            }
        }

        private static IEnumerable<GridPoint> EightSlopes()
        {
            for (var x = -1; x <= 1; x++)
            {
                for (var y = -1; y <= 1; y++)
                {
                    if (x == 0 && y == 0) continue;
                    yield return new GridPoint(x, y);
                }
            }
        }

        private static IEnumerable<char> SurroundingEightSightlines(IDefinedSizeGrid<char> grid, GridPoint center)
        {
            foreach (var slope in EightSlopes())
            {
                for (var dist = 1;; dist++)
                {
                    var point = slope.Scale(dist) + center;
                    if (!grid.Region().ContainsPoint(point))
                    {
                        // We reached the end and didn't see anything
                        // yield return backup;
                        break;
                    }

                    var cell = grid.Get(point);
                    
                    // We can see right through these
                    if (cell == '.') continue;
                    
                    yield return cell;
                    break;
                }
            }
        }
    }
}