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

    private long _stepCounter = 0;

    public IntcodeComputer(IEnumerable<int> nums)
    {
        Nums = nums.ToList();
    }

    public void Step()
    {
        var rawOpcode = Nums[Pc];
        var (opcode, modes) = ParseOpcode(rawOpcode);

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

        _stepCounter++;
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
    
    private static int ParseOpcode(int rawOpcode, Span<int> modes)
    {
        var opcode = rawOpcode % 100;
        var modesInt = rawOpcode / 100;

        var i = 0;
        while (modesInt != 0)
        {
            modes[i] = modesInt % 10;
            i++;
            modesInt /= 10;
        }

        return opcode;
    }

    private int[] FetchArgs(int count, int[] modes)
    {
        var results = new int[count];
        for (int i = 0; i < count; i++)
        {
            results[i] = GetParameter(modes.IndexOrDefault(i), Nums[Pc + i + 1]);
        }

        return results;
    }
    
    private void FetchArgs(int[] modes, Span<int> results)
    {
        for (int i = 0; i < results.Length; i++)
        {
            results[i] = GetParameter(modes.IndexOrDefault(i), Nums[Pc + i + 1]);
        }
    }

    public void RunToCompletion(RunOptions options = null)
    {
        options ??= new RunOptions();
        while (State == ComputerState.Running)
        {
            if (options.Limit >= 0 && _stepCounter >= options.Limit)
            {
                break;
            }
            Step();
            if (_stepCounter % 1000 == 0)
            {
                // Console.Out.WriteLine($"Step {_stepCounter}");
            }
        }
    }

    public class RunOptions
    {
        public long Limit = -1;
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
        Span<int> args = stackalloc int[2];
        FetchArgs(modes, args);
        // var args = FetchArgs(2, modes);
        var result = args[0] + args[1];
        var resLoc = Nums[Pc + 3];
        WriteParameter(modes.IndexOrDefault(2), resLoc, result);
        Pc += 4;
    }

    private void Multiply(int[] modes)
    {
        var args = FetchArgs(2, modes);
        var result = args[0] * args[1];
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
        Pc += 2;
    }
    
    private void OutputInstruction(List<int> modes)
    {
        // Output is always position mode
        var value = GetParameter(0, Nums[Pc + 1]);
        OutputHandler(value);
        Pc += 2;
    }

    // private void JumpIfTrue(List<int> modes)
    // {
    //     var cond = GetParameter(modes.IndexOrDefault(1), Nums[Pc + 1]);
    //     var dest = GetParameter(modes.IndexOrDefault(2), Nums[Pc + 2]);
    // }

    private void Halt()
    {
        State = ComputerState.Halted;
    }
}