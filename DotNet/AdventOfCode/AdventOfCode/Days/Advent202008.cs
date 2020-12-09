using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202008 : Problem
    {
        public Advent202008() : base(2020, 8)
        {
        }

        public override string PartOne(string[] input)
        {
            var instructions = ParseInstructions(input);
            var machine = new HandheldGameMachine
            {
                Program = instructions.ToArray()
            };

            var executedInstructions = new HashSet<int>();
            while (true)
            {
                if (executedInstructions.Contains(machine.ProgramCounter))
                {
                    return machine.Accumulator.ToString();
                }

                executedInstructions.Add(machine.ProgramCounter);
                machine.Step();
            }
        }

        private static IEnumerable<RawInstruction> ParseInstructions(string[] input)
        {
            return input.Select(s =>
            {
                var pieces = s.Split();
                return new RawInstruction
                {
                    Opcode = pieces[0],
                    Argument = int.Parse(pieces[1])
                };
            });
        }

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }

    }
    
    public class RawInstruction
    {
        public string Opcode;
        public int Argument;
    }

    public class HandheldGameMachine
    {
        public int ProgramCounter = 0;
        public RawInstruction[] Program;
        public int Accumulator;

        public void Step()
        {
            var currentInstruction = Program[ProgramCounter];

            switch (currentInstruction.Opcode)
            {
                case "acc":
                    Accumulator += currentInstruction.Argument;
                    ProgramCounter++;
                    break;
                case "jmp":
                    ProgramCounter += currentInstruction.Argument;
                    break;
                case "nop":
                    ProgramCounter++;
                    break;
                default:
                    throw new Exception($"Illegal opcode: {currentInstruction.Opcode}");
            }
        }
    }
}