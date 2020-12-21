using AdventOfCode.AdventLib.Grid;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Grid
{
    public class DefinedSizeGridTests
    {
        [TestCase(0, 0, 'J')]
        [TestCase(2, 2, 'F')]
        [TestCase(3, 2, 'C')]
        [TestCase(0, 2, 'L')]
        public void ShouldRotate(int x, int y, char expected)
        {
            var solidGrid = new SolidGrid<char>(new[]
            {
                new[] {'A', 'B', 'C'},
                new[] {'D', 'E', 'F'},
                new[] {'G', 'H', 'I'},
                new[] {'J', 'K', 'L'}
            });

            var rotated = solidGrid.RotateClockwise();

            rotated.Get(new GridPoint(x, y)).Should().Be(expected);
        }
    }
}