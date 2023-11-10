using AdventOfCode.AdventLib.Grid;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Grid;

public class ShiftedGridTests
{
    [Test]
    public void ShouldShiftGrid()
    {
        var shifted = CommonGrids.CoordinateGrid.Shift(new GridPoint(10, 10));
        shifted.Get(new GridPoint(10, 10)).Should().Be(new GridPoint(20, 20));
    }
}