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

        [Test]
        public void ShouldNotSupportAccessOutsideOfWindow()
        {
            var region = new GridRegion(new GridPoint(-10, -10), 20, 20);
            var windowed = CommonGrids.CoordinateGrid.Windowed(region);


            var outsideNegative = () => windowed.Get(new GridPoint(-10, -10));
            var inside = () => windowed.Get(new GridPoint(10, 10));

            outsideNegative.Should().Throw<NonexistentCellException>();
            inside.Should().NotThrow();
        }

        [Test]
        public void ShouldAllowNegativeWindows()
        {
            var region = new GridRegion(new GridPoint(-10, -10), 20, 20);
            var windowed = CommonGrids.CoordinateGrid.Windowed(region);

            windowed.Get(new GridPoint(0, 0)).Should().Be(new GridPoint(-10, -10));
            windowed.Get(new GridPoint(11, 11)).Should().Be(new GridPoint(1, 1));
        }
    }
}