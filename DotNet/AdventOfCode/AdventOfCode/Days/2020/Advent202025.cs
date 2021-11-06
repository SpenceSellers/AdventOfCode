using System.Linq;
using AdventOfCode.AdventLib;

namespace AdventOfCode.Days._2020
{
    public class Advent202025 : Problem
    {
        public override object PartOne(string[] input)
        {
            var (doorKey, cardKey) = input.Select(long.Parse);
            var cardKeyLoopCount = FindLoopCount(cardKey);

            var key = ApplyEncryption(doorKey, cardKeyLoopCount);

            return key;
        }

        private static long FindLoopCount(long searchValue)
        {
            var value = 1L;
            for (var i = 1;; i++)
            {
                value = LoopStep(value);
                if (value == searchValue)
                {
                    return i;
                }
            }
        }

        private static long ApplyEncryption(long subjectNumber, long count)
        {
            var value = 1L;
            for (int i = 0; i < count; i++)
            {
                value = LoopStep(value, subjectNumber);
            }

            return value;
        }

        private static long LoopStep(long value, long subjectNumber = 7)
        {
            const long divNum = 20201227L;
            value *= subjectNumber;
            value %= divNum;
            return value;
        }

        public override object PartTwo(string[] input)
        {
            return 1;
        }
    }
}