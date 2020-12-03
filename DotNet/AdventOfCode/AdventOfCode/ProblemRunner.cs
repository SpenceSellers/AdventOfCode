using System;
using System.IO;

namespace AdventOfCode
{
    public class ProblemRunner
    {
        private bool _runPartOne = true;
        private bool _runPartTwo = true;
        private string _inputPrefix = "input";

        public ProblemRunner SkipPartOne()
        {
            _runPartOne = false;
            return this;
        }

        public ProblemRunner SkipPartTwo()
        {
            _runPartTwo = false;
            return this;
        }

        public ProblemRunner UseSampleInput()
        {
            _inputPrefix = "sample";
            return this;
        }

        public void Run(Problem problem)
        {
            var inputDir = Environment.GetEnvironmentVariable("INPUT_DIR") ?? Environment.CurrentDirectory;
            var path = $"{inputDir}/{_inputPrefix}-{problem.Year}-{problem.Day}.txt";
            var lines = File.ReadAllLines(path);

            try
            {
                var result = problem.PartOne(lines);
                Console.WriteLine($"⭐\n{result}");
            }
            catch (NotImplementedException e)
            {
                
            }
            
            try
            {
                var result = problem.PartTwo(lines);
                Console.WriteLine($"⭐⭐\n{result}");
            }
            catch (NotImplementedException e)
            {
                
            }
        }
    }
}