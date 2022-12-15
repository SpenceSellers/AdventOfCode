using AdventOfCode.Days._2019;
using AdventOfCode.Days._2020;
using AdventOfCode.Days._2021;
using AdventOfCode.Days._2022;

namespace AdventOfCode
{
    class Program
    {
        static void Main(string[] args)
        {
            new ProblemRunner()
                // .UseSampleInput()
                .SkipPartOne()
                .RunTimes(20)
                .Run(new Advent202212());
        }
    }
}