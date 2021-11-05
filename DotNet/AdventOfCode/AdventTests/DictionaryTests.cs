using System.Collections.Generic;
using AdventOfCode.AdventLib;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests
{
    public class DictionaryTests
    {
        [Test]
        public void ShouldIncrement()
        {
            var dict = new Dictionary<string, int> {{"existing", 100}};
            dict.Increment("non_existing");
            dict.Increment("non_existing");
            dict.Increment("existing");

            dict["existing"].Should().Be(101);
            dict["non_existing"].Should().Be(2);

            dict.Increment("existing", 50);
            dict["existing"].Should().Be(151);
        }
    }
}