using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days
{
    public class Advent202015 : Problem
    {
        public Advent202015() : base(2020, 15)
        {
        }

        public override string PartOne(string[] input)
        {
            var numbers = input[0].Split(',').Select(int.Parse).ToList();
            var spoken = numbers.Take(numbers.Count - 1).ToList();
            var lastSpoken = numbers.Last();
            while (true)
            {
                var turnNumber = spoken.Count + 2;
                var lastSpokenIndex = spoken.LastIndexOf(lastSpoken);
                if (lastSpokenIndex == -1)
                {
                    spoken.Add(lastSpoken);
                    lastSpoken = 0;
                }
                else
                {
                    var lastSpokenTurn = spoken.Count + 1;
                    var prevSpokenTurn = lastSpokenIndex + 1;
                    spoken.Add(lastSpoken);
                    lastSpoken = (lastSpokenTurn - prevSpokenTurn);
                }
                
                // Console.Out.WriteLine($"{turnNumber}: {lastSpoken}");
                if (turnNumber % 100_000 == 0)
                {
                    Console.Out.WriteLine($"{turnNumber}");
                }

                // if (turnNumber == 30_000_000)
                if (turnNumber == 2020)
                {
                    return lastSpoken.ToString();
                }
            }
        }

        public override string PartTwo(string[] input)
        {
            var numbers = input[0].Split(',').Select(int.Parse).ToList();
            var spoken = numbers.Take(numbers.Count - 1).ToList();
            var lastSpoken = numbers.Last();
            var spokenBefore = new Dictionary<int, int>();
            // Pre-seed beginning
            foreach (var (num, index) in spoken.Select((s, i) => (s, i)))
            {
                spokenBefore[num] = index + 1;
            }
            while (true)
            {
                var turnNumber = spoken.Count + 2;
                if (!spokenBefore.ContainsKey(lastSpoken))
                {
                    spokenBefore[lastSpoken] = turnNumber - 1;
                    spoken.Add(lastSpoken);
                    lastSpoken = 0;
                }
                else
                {
                    var lastTimeSpoken = spokenBefore[lastSpoken];
                    spokenBefore[lastSpoken] = turnNumber - 1;
                    var lastSpokenTurn = spoken.Count + 1;
                    var prevSpokenTurn = lastTimeSpoken;
                    spoken.Add(lastSpoken);
                    lastSpoken = (lastSpokenTurn - prevSpokenTurn);
                }
                
                // Console.Out.WriteLine($"{turnNumber}: {lastSpoken}");
                if (turnNumber % 100_000 == 0)
                {
                    Console.Out.WriteLine($"{turnNumber}");
                }

                if (turnNumber == 30_000_000)
                // if (turnNumber == 2020)
                {
                    return lastSpoken.ToString();
                }

            }
        }

        // private class ElfGame
        // {
        //     
        // }
    }
}