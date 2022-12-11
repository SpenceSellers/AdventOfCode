using System;
using System.Linq;
using AdventOfCode.AdventLib;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Spans;

public class SpanSplitTests
{
    [Test]
    public void ShouldSplit()
    {
        var s = "green red blue";
        var splits = s.AsMemory().MemorySplit(' ').ToArray();
        splits.Length.Should().Be(3);
        splits[0].ToString().Should().Be("green");
        splits[1].ToString().Should().Be("red");
        splits[2].ToString().Should().Be("blue");
    }

    [Test]
    public void ShouldSplitEmptyEnd()
    {
        var s = "green red blue ";
        var splits = s.AsMemory().MemorySplit(' ').ToArray();
        splits.Length.Should().Be(4);
        splits[2].ToString().Should().Be("blue");
        splits[3].ToString().Should().Be("");
    }

    [Test]
    public void ShouldSplitEmpty()
    {
        var s = "";
        var splits = s.AsMemory().MemorySplit(' ').ToArray();
        splits.Length.Should().Be(1);
        splits[0].ToString().Should().Be("");
    }

    [Test]
    public void ShouldSplitEmptyBegin()
    {
        var s = " green red blue ";
        var splits = s.AsMemory().MemorySplit(' ').ToArray();
        splits[0].ToString().Should().Be("");
        splits[1].ToString().Should().Be("green");
    }
    
}