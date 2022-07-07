using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Authentication;
using System.Text;

namespace AdventOfCode.AdventLib.Intcode;

public enum ComputerState
{
    Running,
    Halted
}

public class IntcodeComputer
{
    public List<int> Nums { get; }
    public int Pc { get; set; }
    public ComputerState State { get; private set; } = ComputerState.Running;

    public IntcodeComputer(IEnumerable<int> nums)
    {
        Nums = nums.ToList();
    }

    public void Step()
    {
        var opcode = Nums[Pc];
        // PrintDebugInfo();

        switch (opcode)
        {
            case 1:
                Add();
                break;
            case 2:
                Multiply();
                break;
            case 99:
                Halt();
                break;
            default:
                Console.Out.WriteLine($"Unknown opcode {opcode}");
                throw new Exception("Unknown opcode");
        }
    }

    public void RunToCompletion()
    {
        while (State == ComputerState.Running)
        {
            Step();
        }
    }

    private void PrintDebugInfo()
    {
        Console.Out.WriteLine();
        var lineOne = new StringBuilder();
        var lineTwo = new StringBuilder();
        var lineThree = new StringBuilder();
        for (int i = 0; i < Nums.Count; i++)
        {
            lineOne.Append(i == Pc ? "XXXXX" : "     ");
            lineTwo.Append(i.ToString().PadRight(5));
            lineThree.Append(Nums[i].ToString().PadRight(5));
        }
        Console.Out.WriteLine(lineOne);
        Console.Out.WriteLine(lineTwo);
        Console.Out.WriteLine(lineThree);
    }

    private void Add()
    {
        var result = Nums[Nums[Pc + 1]] + Nums[Nums[Pc + 2]];
        var resLoc =Nums[Pc + 3];
        Nums[resLoc] = result;
        Pc += 4;
    }

    private void Multiply()
    {
        var result = Nums[Nums[Pc + 1]] * Nums[Nums[Pc + 2]];
        var resLoc = Nums[Pc + 3];
        Nums[resLoc] = result;
        Pc += 4;
    }

    private void Halt()
    {
        State = ComputerState.Halted;
    }
}