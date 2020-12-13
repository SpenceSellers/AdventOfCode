using AdventOfCode.AdventLib.Grid;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests
{
    public class GridPointTests
    {
        [Test]
        public void ShouldRotateCorrectly()
        {
            var initial = new GridPoint(10, 20);
            initial.RotateAroundOrigin(4).Should().Be(initial);
            initial.RotateAroundOrigin(-4).Should().Be(initial);
            initial.RotateAroundOrigin(2).Should().Be(new GridPoint(-10, -20));
        }
    }
}