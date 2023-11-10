using AdventOfCode.Days._2022;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Days._2022;

public class Advent202209Tests
{
    private string _sampleInput = """
        R 4
        U 4
        L 3
        D 1
        R 4
        D 1
        L 5
        R 2
        """;

    [Test]
    public void ShouldAnswerPart1()
    {
        var result = new Advent202209().PartOne(_sampleInput.Split("\n"));
        result.Should().Be(13);
    }
}