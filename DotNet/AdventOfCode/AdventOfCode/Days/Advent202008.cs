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

        public int? RunUntilTermination(HandheldGameMachine machine)
        {
            var executedInstructions = new HashSet<int>();
            while (true)
            {
                if (machine.ProgramCounter == machine.Program.Length)
                {
                    // This program has completed
                    return machine.Accumulator;
                }
                
                if (executedInstructions.Contains(machine.ProgramCounter))
                {
                    // Nah we're in an infinite loop.
                    return null;
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
            var instructions = ParseInstructions(input);
            var machines = MutatedPrograms(instructions.ToList())
                .Select(program => new HandheldGameMachine {Program = program});

            return (machines.Select(RunUntilTermination)
                .Where(result => result != null)
                .Select(result => result.ToString())).FirstOrDefault();
        }


        private IEnumerable<RawInstruction[]> MutatedPrograms(IList<RawInstruction> baseProgram)
        {
            for (var i = 0; i < baseProgram.Count; i++)
            {
                var instruction = baseProgram[i];
                var newInstruction = instruction.Opcode switch
                {
                    "nop" => new RawInstruction {Opcode = "jmp", Argument = instruction.Argument},
                    "jmp" => new RawInstruction {Opcode = "nop", Argument = instruction.Argument},
                    _ => null
                };

                if (newInstruction == null) continue;
                
                var newProgram = baseProgram.ToArray();
                newProgram[i] = newInstruction;
                yield return newProgram;
            }
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