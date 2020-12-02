using System.Linq;
using AdventOfCode;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests
{
    public class EnumerableTests
    {
        [Test]
        public void ChunksShouldChunk()
        {
            var l = new[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
            var result = l.Chunks(3).ToList();
            result.Should().HaveCount(4);
            result[0].Should().BeEquivalentTo(new[] {1, 2, 3});
            result[3].Should().BeEquivalentTo(new[] {10});
        }
    }
}