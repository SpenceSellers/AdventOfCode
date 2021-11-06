using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Days._2020
{
    public class Advent202015 : Problem
    {
        public Advent202015() : base(2020, 15)
        {
        }

        public override object PartOne(string[] input)
        {
            var numbers = input[0].Split(',').Select(int.Parse).ToList();
            return SolveTo(numbers, 2020);
        }

        public override object PartTwo(string[] input)
        {
            var numbers = input[0].Split(',').Select(int.Parse).ToList();
            return SolveTo(numbers, 30_000_000);
        }

        private static string SolveTo(IReadOnlyCollection<int> numbers, int targetNumber)
        {
            var spoken = numbers.Take(numbers.Count - 1).ToList();
            var lastSpoken = numbers.Last();
            var spokenBefore = new Dictionary<int, int>();
            // Pre-seed beginning
            foreach (var (num, index) in spoken.Select((s, i) => (s, i)))
            {
                spokenBefore[num] = index + 1;
            }

            var turnNumber = spoken.Count + 2;
            while (true)
            {
                if (!spokenBefore.ContainsKey(lastSpoken))
                {
                    spokenBefore[lastSpoken] = turnNumber - 1;
                    lastSpoken = 0;
                }
                else
                {
                    var lastTimeSpoken = spokenBefore[lastSpoken];
                    spokenBefore[lastSpoken] = turnNumber - 1;
                    var lastSpokenTurn = turnNumber - 1;
                    lastSpoken = lastSpokenTurn - lastTimeSpoken;
                }

                if (turnNumber % 1_000_000 == 0)
                {
                    // Progress indication
                    Console.Out.WriteLine($"... {turnNumber}");
                }

                if (turnNumber == targetNumber)
                {
                    return lastSpoken.ToString();
                }

                turnNumber++;
            }
        }
    }
}