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

    [Test]
    public void ShouldUseImmediateMode()
    {
        var input = new[] { 1002, 4, 3, 4, 33 };
        var computer = new IntcodeComputer(input);
        computer.Step();
        computer.Nums[4].Should().Be(99);
    }

    [Test]
    public void ShouldSupportNegativeNumbers()
    {
        var input = new[] {1101,100,-1,4,0};
        var computer = new IntcodeComputer(input);
        computer.Step();
        computer.Nums[4].Should().Be(99);
    }

    [Test]
    public void ShouldSupportInput()
    {
        var input = new[] {3, 0};
        var computer = new IntcodeComputer(input);
        computer.InputHandler = () => 42;
        computer.Step();
        computer.Nums[0].Should().Be(42);
    }
    
    [Test]
    public void ShouldSupportOutput()
    {
        var input = new[] {4, 0};
        var computer = new IntcodeComputer(input);
        var returned = 0;
        computer.OutputHandler = (x) => returned = x;
        computer.Step();
        returned.Should().Be(4);
    }
}