using AdventOfCode.AdventLib.Grid;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Grid
{
    public class GridPointTests
    {
        [Test]
        public void PointEquality()
        {
            new GridPoint(20, 25).Should().Be(new GridPoint(20, 25));
            new GridPoint(20, 25).Should().NotBe(new GridPoint(20, 28));
        }
        
        [Test]
        public void PointHashing()
        {
            new GridPoint(20, 25).GetHashCode().Should().Be(new GridPoint(20, 25).GetHashCode());
            new GridPoint(20, 25).GetHashCode().Should().NotBe(new GridPoint(20, 28).GetHashCode());
        }
    }
}