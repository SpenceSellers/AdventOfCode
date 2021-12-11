using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using AdventOfCode.AdventLib;
using AdventOfCode.AdventLib.Grid;

namespace AdventOfCode.Days._2021
{
    public class Advent202111 : Problem
    {
        public override object PartOne(string[] input)
        {
            var octopi = input.ToGrid().Map(x => int.Parse(x.ToString()));
            var totalFlashed = 0;
            for (var i = 0; i < 100; i++)
            {

                var (nextState, flashed) = Step(octopi);
                octopi = nextState;
                totalFlashed += flashed;
            }

            return totalFlashed;
        }

        private (IDefinedSizeGrid<int>, int) Step(IDefinedSizeGrid<int> grid)
        {
            var nextState = grid.Map(x => x + 1).Solidify();
            var haveFlashed = new HashSet<GridPoint>();
            var didFlash = true;
            while (didFlash)
            {
                didFlash = false;
                var flashing = nextState
                    .AllEntries()
                    .Where2(i => i > 9)
                    .Where1(x => !haveFlashed.Contains(x));
                foreach (var (point, _) in flashing)
                {
                    didFlash = true;
                    haveFlashed.Add(point);
                    foreach (var flashPoint in point.Adjacent8)
                    {
                        if (!nextState.Region().ContainsPoint(flashPoint)) continue;
                        nextState.Set(flashPoint, nextState.Get(flashPoint) + 1);
                    }
                }
            }

            foreach (var flashPoint in haveFlashed)
            {
                nextState.Set(flashPoint, 0);
            }

            return (nextState, haveFlashed.Count);
        }

        public override object PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }
    }
}