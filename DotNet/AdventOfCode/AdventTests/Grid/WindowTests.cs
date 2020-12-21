using AdventOfCode.AdventLib.Grid;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Grid
{
    public class WindowTests
    {
        [Test]
        public void TestWindowing()
        {
            var solidGrid = new SolidGrid<char>(new[]
            {
                new[] {'A', 'B', 'C'},
                new[] {'D', 'E', 'F'},
                new[] {'G', 'H', 'I'},
                new[] {'J', 'K', 'L'}
            });

            var windowed = solidGrid.Windowed(new GridRegion(new GridPoint(1, 2), 2, 2));

            windowed.Get(new GridPoint(0, 0)).Should().Be('H');
            windowed.Get(new GridPoint(1, 1)).Should().Be('L');
            windowed.Get(new GridPoint(1, 0)).Should().Be('I');
        }
    }
}