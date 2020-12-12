using System;
using System.Collections.Generic;
using System.Linq;
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
            var initial = SolidGrid<char>.Extract(input);
            var states = new List<IDefinedSizeGrid<char>> {initial};
            
            while (true)
            {
                Console.Out.WriteLine("step");
                var prevState = states.Last();
                var newState = Step(prevState).Solidify();
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

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }

        public static IDefinedSizeGrid<char> Step(IDefinedSizeGrid<char> chars)
        {
            return CommonGrids.CoordinateGrid
                .Map(coord => (chars.Get(coord), SurroundingEight(chars, coord, '.')))
                .Map(x => StepFunction(x.Item2, x.Item1))
                .Windowed(chars.Region());
        }

        public static char StepFunction(IEnumerable<char> surround, char self)
        {
            return self switch
            {
                '.' => '.',
                '#' => surround.Count(c => c == '#') >= 4 ? 'L' : '#',
                'L' => surround.Any(c => c == '#') ? 'L' : '#'
            };
        }
        
        public static IEnumerable<char> SurroundingEight(IDefinedSizeGrid<char> grid, GridPoint p, char backup)
        {
            for (var x = -1; x <= 1; x++)
            {
                for (var y = -1; y <= 1; y++)
                {
                    if (x == 0 && y == 0) continue;
                    var offset = new GridPoint(x, y);
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
        }
    }
}