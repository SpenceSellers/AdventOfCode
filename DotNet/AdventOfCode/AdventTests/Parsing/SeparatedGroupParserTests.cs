using System.Linq;
using AdventOfCode.Parsing;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Parsing
{
    public class SeparatedGroupParserTests
    {
        [Test]
        public void ShouldParseSeparateGroups()
        {
            var input = new[]
            {
                "g1 one",
                "g1 two",
                "g1 three",
                "",
                "g2 one",
                "",
                "g3 one",
                "g3 two"
            };

            var groups = new SeparatedGroupParser().Parse(input).Select(x => x.ToList()).ToList();

            groups.Should().HaveCount(3);
            groups[0].Should().Contain("g1 two");
            groups[2].Should().Contain("g3 two");
        }

        [Test]
        public void ShouldAllowNewlineAtEnd()
        {
            var input = new[]
            {
                "g1 one",
                "g1 two",
                "g1 three",
                "",
                "g2 one",
                "",
                "g3 one",
                "g3 two",
                ""
            };

            var groups = new SeparatedGroupParser().Parse(input).Select(x => x.ToList()).ToList();

            groups.Should().HaveCount(3);
            groups[0].Should().Contain("g1 two");
            groups[2].Should().Contain("g3 two");
        }
    }
}