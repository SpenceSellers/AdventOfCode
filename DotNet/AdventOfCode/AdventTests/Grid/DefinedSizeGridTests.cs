using System.Linq;
using AdventOfCode.AdventLib;
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

        [Test]
        public void ShouldGetColumns()
        {
            var solidGrid = new SolidGrid<char>(new[]
            {
                new[] {'A', 'B', 'C'},
                new[] {'D', 'E', 'F'},
                new[] {'G', 'H', 'I'},
                new[] {'J', 'K', 'L'}
            });

            var columns = solidGrid.Columns().Realize();
            columns.Count.Should().Be(3);
            columns[1].Should().Equal('B', 'E', 'H', 'K');
        }

        [Test]
        public void ShouldGetRows()
        {
            var solidGrid = new SolidGrid<char>(new[]
            {
                new[] {'A', 'B', 'C'},
                new[] {'D', 'E', 'F'},
                new[] {'G', 'H', 'I'},
                new[] {'J', 'K', 'L'}
            });

            var rows= solidGrid.Rows().Realize();
            rows.Count.Should().Be(4);
            rows[1].Should().Equal('D', 'E', 'F');
        }
    }
}