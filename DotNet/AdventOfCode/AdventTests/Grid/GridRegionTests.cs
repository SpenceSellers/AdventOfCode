using AdventOfCode.AdventLib.Grid;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Grid
{
    public class GridRegionTests
    {
        // Identical
        [TestCase(30, 20, 10, 15, true)]
        // Completely contained
        [TestCase(30, 20, 1, 1, true)]
        [TestCase(30, 22, 1, 1, true)]
        // Tricky
        [TestCase(30, 34, 1, 1, true)]
        [TestCase(30, 35, 1, 1, false)]
        // Partial intersection
        [TestCase(29, 20, 2, 2, false)]
        [TestCase(30, 19, 2, 2, false)]
        [TestCase(30, 20, 11, 1, false)]
        [TestCase(30, 20, 2, 16, false)]
        // No intersection
        [TestCase(5, 20, 2, 2, false)]
        [TestCase(30, 5, 2, 2, false)]
        [TestCase(50, 20, 2, 2, false)]
        [TestCase(30, 50, 2, 2, false)]
        public void ShouldContainRegion(int x, int y, int width, int height, bool shouldContain)
        {
            var baseRegion = new GridRegion(new GridPoint(30, 20), 10, 15);

            var region = new GridRegion(new GridPoint(x, y), width, height);

            baseRegion.ContainsRegion(region).Should().Be(shouldContain);
        }
    }
}