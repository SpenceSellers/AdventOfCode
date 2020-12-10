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
            result[0].Should().Equal(1, 2, 3);
            result[3].Should().Equal(10);
        }

        [Test]
        public void CompleteChunksShouldDiscardEnd()
        {
            var l = new[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
            var result = l.CompleteChunks(3).ToList();
            result.Should().HaveCount(3);
            result[0].Should().Equal(1, 2, 3);
            result[2].Should().Equal(7, 8, 9);
        }

        [Test]
        public void CanRotate()
        {
            var l = new[] {1, 2, 3, 4, 5, 6};
            l.Rotate(-2).Should().Equal(3, 4, 5, 6, 1, 2);
            l.Rotate(2).Should().Equal(5, 6, 1, 2, 3, 4);
            l.Rotate(0).Should().Equal(l);
            l.Rotate(6).Should().Equal(l);
            l.Rotate(-6).Should().Equal(l);
            l.Rotate(12).Should().Equal(l);
            l.Rotate(-12).Should().Equal(l);
            l.Rotate(14).Should().Equal(5, 6, 1, 2, 3, 4);
            l.Rotate(10).Should().Equal(3, 4, 5, 6, 1, 2);
        }

        [Test]
        public void CombinationsShouldWork()
        {
            var l = new[] {1, 2, 3, 4};
            var result = l.Combinations(2).ToList();

            var answers = new[]
            {
                new [] {1, 2},
                new [] {1, 3},
                new [] {1, 4},
                new [] {2, 3},
                new [] {2, 4},
                new [] {3, 4}
            };

            result.Should().BeEquivalentTo(answers);
        }

        [Test]
        public void ProductShouldMultiplyCorrectly()
        {
            new[] {1, 2, 3}.Product().Should().Be(6);
            new int[] { }.Product().Should().Be(1);
            new[] {1, 2, 0, 5, 100, 200}.Product().Should().Be(0);
        }

        [Test]
        public void GetSequencesShouldWorkCorrectly()
        {
            var list = new[] {1, 2, 3, 4, 5, 6};
            var sequences = list.SequencesOfSize(4).ToList();
            sequences.Should().HaveCount(3);
            sequences.Last().Should().Contain(6);
            sequences.First().Should().NotContain(6);
            sequences.ForEach(seq => seq.Should().HaveCount(4));
        }
    }
}