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
    // public List<long> Nums { get; }
    public Dictionary<long, long> Nums;

    public IList<long> LinearNums
    {
        get
        {
            var maxKey = Nums.Keys.Max();
            var result = new long[maxKey + 1];
            foreach (var (i, v) in Nums)
            {
                result[i] = v;
            }

            return result;
        }
    }

    public int Pc { get; set; }
    public ComputerState State { get; private set; } = ComputerState.Running;
    public long RelativeBase { get; set; }= 0;

    public Func<long> InputHandler = () => throw new ApplicationException("Input handler is not defined");
    public Action<long> OutputHandler = (long x) => throw new ApplicationException("Output handler is not defined");

    private long _stepCounter = 0;

    public IntcodeComputer(IEnumerable<long> nums)
    {
        Nums = nums.WithIndex().ToDictionary(x => (long) x.index, x => x.value);
    }

    public IntcodeComputer(IEnumerable<int> nums)
    {
        Nums = nums.WithIndex().ToDictionary(x => (long) x.index, x => (long) x.value);
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
            case 5:
                JumpIfTrue(modes);
                break;
            case 6:
                JumpIfFalse(modes);
                break;
            case 7:
                LessThan(modes);
                break;
            case 8:
                Equals(modes);
                break;
            case 9:
                AdjustRelativeBase(modes);
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

    private static (long opcode, List<long> modes) ParseOpcode(long rawOpcode)
    {
        var opcode = rawOpcode % 100;
        var modesInt = rawOpcode / 100;

        var modes = new List<long>();
        while (modesInt != 0)
        {
            modes.Add(modesInt % 10);
            modesInt /= 10;
        }

        return (opcode, modes);
    }

    private static long ParseOpcode(long rawOpcode, Span<long> modes)
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

    private long[] FetchArgs(int count, IList<long> modes)
    {
        var results = new long[count];
        for (var i = 0; i < count; i++)
        {
            results[i] = GetParameter(modes.IndexOrDefault(i), Nums[Pc + i + 1]);
        }

        return results;
    }

    private void FetchArgs(long[] modes, Span<long> results)
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

    private long GetParameter(long mode, long value)
    {
        return mode switch
        {
            // Position
            0 => Nums[value],
            // Immediate
            1 => value,
            2 => Nums[value + RelativeBase],
            _ => throw new ArgumentException("Unknown mode " + mode)
        };
    }

    private void WriteParameter(long mode, long parameterValue, long resultValue)
    {
        if (mode != 0)
        {
            throw new ArgumentException($"Invalid mode for write: {mode}");
        }

        Nums[(int) parameterValue] = resultValue;
    }

    private void Add(long[] modes)
    {
        Span<long> args = stackalloc long[2];
        FetchArgs(modes, args);
        // var args = FetchArgs(2, modes);
        var result = args[0] + args[1];
        var resLoc = Nums[Pc + 3];
        WriteParameter(modes.IndexOrDefault(2), resLoc, result);
        Pc += 4;
    }

    private void Multiply(long[] modes)
    {
        var args = FetchArgs(2, modes);
        var result = args[0] * args[1];
        var resLoc = Nums[Pc + 3];
        WriteParameter(modes.IndexOrDefault(2), resLoc, result);
        Pc += 4;
    }

    private void InputInstruction(List<long> modes)
    {
        var input = InputHandler();
        modes.JsonTrace("Modes input");
        // This doesn't work:
        // var pos = FetchArgs(1, modes);
        // Input pos is always immediate mode
        // var pos = GetParameter(1, Nums[Pc + 1]);
        var args = FetchArgs(1, modes);
        // args.JsonTrace("Args");
        // pos.JsonTrace("Pos original");
        WriteParameter(0, args[0], input);
        Pc += 2;
    }

    private void OutputInstruction(List<long> modes)
    {
        // Output is always position mode
        var value = FetchArgs(1, modes);
        // var value = GetParameter(0, Nums[Pc + 1]);
        OutputHandler(value[0]);
        Pc += 2;
    }

    private void JumpIfTrue(IList<long> modes)
    {
        var args = FetchArgs(2, modes);
        if (args[0] != 0)
        {
            Pc = (int) args[1];
        }
        else
        {
            Pc += 3;
        }
    }

    private void JumpIfFalse(IList<long> modes)
    {
        var args = FetchArgs(2, modes);
        if (args[0] == 0)
        {
            Pc = (int) args[1];
        }
        else
        {
            Pc += 3;
        }
    }

    private void LessThan(IList<long> modes)
    {
        var args = FetchArgs(2, modes);
        if (args[0] < args[1])
        {
            Nums[(int) Nums[Pc + 3]] = 1;
        }
        else
        {
            Nums[(int) Nums[Pc + 3]] = 0;
        }
        Pc += 4;
    }

    private void Equals(IList<long> modes)
    {
        var args = FetchArgs(2, modes);
        if (args[0] == args[1])
        {
            Nums[(int) Nums[Pc + 3]] = 1;
        }
        else
        {
            Nums[(int) Nums[Pc + 3]] = 0;
        }

        Pc += 4;
    }

    private void AdjustRelativeBase(IList<long> modes)
    {
        var args = FetchArgs(1, modes);
        RelativeBase += args[0];
        Pc += 2;
    }

    private void Halt()
    {
        State = ComputerState.Halted;
    }
}