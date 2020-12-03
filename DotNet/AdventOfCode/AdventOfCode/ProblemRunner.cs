using System;
using System.Diagnostics;
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

            if (_runPartOne) RunProblem("⭐", () => problem.PartOne(lines));
            if (_runPartTwo) RunProblem("⭐⭐", () => problem.PartTwo(lines));
        }

        private void RunProblem(string title, Func<string> func)
        {
            try
            {
                var stopwatch = new Stopwatch();
                stopwatch.Start();
                var result = func();
                stopwatch.Stop();
                Console.WriteLine($"== {title} ==");
                Console.WriteLine($"Complete in {stopwatch.ElapsedMilliseconds} ms");
                Console.WriteLine(result);
            }
            catch (NotImplementedException e)
            {
                
            }
        }
    }
}