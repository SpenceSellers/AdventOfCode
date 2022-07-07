using AdventOfCode.AdventLib.Intcode;
using FluentAssertions;
using NUnit.Framework;

namespace AdventTests.Intcode;

public class IntcodeComputerTests
{
    [Test]
    public void ShouldRunBasicProgram()
    {
        var input = new[] { 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 };
        var computer = new IntcodeComputer(input);
        computer.RunToCompletion();
        computer.Nums.Should().BeEquivalentTo(new[] { 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 });
    }

    [Test]
    public void ShouldRunASingleStep()
    {
        var input = new[] { 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 };
        var computer = new IntcodeComputer(input);
        computer.Step();
        computer.Nums.Should().BeEquivalentTo(new[] { 1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 });
    }
}