using AdventOfCode.AdventLib.Grid;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Grid
{
    public class WarpTests
    {
        [TestCase(0, 0, 'A')]
        [TestCase(100, 1000, 'E')]
        [TestCase(0, 2000, 'G')]
        public void TestWarp(int x, int y, char expected)
        {
            var solidGrid = new SolidGrid<char>(new[]
            {
                new[] {'A', 'B', 'C'},
                new[] {'D', 'E', 'F'},
                new[] {'G', 'H', 'I'},
                new[] {'J', 'K', 'L'}
            });

            var warped = solidGrid.Warp(gp => new GridPoint(gp.X / 100, gp.Y / 1000));

            warped.Get(new GridPoint(x, y)).Should().Be(expected);
        }
    }
}