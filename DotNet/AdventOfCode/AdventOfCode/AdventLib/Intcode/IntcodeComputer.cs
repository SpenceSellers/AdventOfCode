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

    public Func<int> InputHandler = () => throw new ApplicationException("Input handler is not defined");
    public Action<int> OutputHandler = (int x) => throw new ApplicationException("Output handler is not defined");

    public IntcodeComputer(IEnumerable<int> nums)
    {
        Nums = nums.ToList();
    }

    public void Step()
    {
        var rawOpcode = Nums[Pc];
        var (opcode, modes) = ParseOpcode(rawOpcode);
        modes.JsonTrace();

        switch (opcode)
        {
            case 1:
                Add(modes.ToArray());
                break;
            case 2:
                Multiply(modes.ToArray());
                break;
            case 3:
                InputInstruction(modes);
                break;
            case 4:
                OutputInstruction(modes);
                break;
            case 99:
                Halt();
                break;
            default:
                Console.Out.WriteLine($"Unknown opcode {opcode}");
                throw new Exception($"Unknown opcode {opcode}");
        }
    }

    private static (int opcode, List<int> modes) ParseOpcode(int rawOpcode)
    {
        var opcode = rawOpcode % 100;
        var modesInt = rawOpcode / 100;

        var modes = new List<int>();
        while (modesInt != 0)
        {
            modes.Add(modesInt % 10);
            modesInt /= 10;
        }

        return (opcode, modes);
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

    private int GetParameter(int mode, int value)
    {
        return mode switch
        {
            // Position
            0 => Nums[value],
            // Immediate
            1 => value,
            _ => throw new ArgumentException("Unknown mode " + mode)
        };
    }

    private void WriteParameter(int mode, int parameterValue, int resultValue)
    {
        if (mode != 0)
        {
            throw new ArgumentException($"Invalid mode for write: {mode}");
        }

        Nums[parameterValue] = resultValue;
    }

    private void Add(int[] modes)
    {
        var a = GetParameter(modes.IndexOrDefault(0), Nums[Pc + 1]);
        var b = GetParameter(modes.IndexOrDefault(1), Nums[Pc + 2]);
        var result = a + b;
        var resLoc = Nums[Pc + 3];
        WriteParameter(modes.IndexOrDefault(2), resLoc, result);
        Pc += 4;
    }

    private void Multiply(int[] modes)
    {
        var a = GetParameter(modes.IndexOrDefault(0), Nums[Pc + 1]);
        var b = GetParameter(modes.IndexOrDefault(1), Nums[Pc + 2]);
        var result = a * b;
        var resLoc = Nums[Pc + 3];
        WriteParameter(modes.IndexOrDefault(2), resLoc, result);
        Pc += 4;
    }

    private void InputInstruction(List<int> modes)
    {
        var input = InputHandler();
        // Input pos is always immediate mode
        var pos = GetParameter(1, Nums[Pc + 1]);
        WriteParameter(0, pos, input);
    }
    
    private void OutputInstruction(List<int> modes)
    {
        // Output is always position mode
        var value = GetParameter(0, Nums[Pc + 1]);
        OutputHandler(value);
    }

    private void Halt()
    {
        State = ComputerState.Halted;
    }
}