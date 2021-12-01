using System;
using System.Diagnostics;
using System.IO;

namespace AdventOfCode
{

    public class ProblemRunner
    {
        public enum ProblemInputSource
        {
            Input,
            Sample
        }
        private bool _runPartOne = true;
        private bool _runPartTwo = true;
        private ProblemInputSource _inputSource = ProblemInputSource.Input;

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
            _inputSource = ProblemInputSource.Sample;
            return this;
        }

        public void Run(Problem problem)
        {
            var inputPath = FilePath(problem, "input");
            var samplePath = FilePath(problem, "sample");
            EnsureFileExists(inputPath);
            EnsureFileExists(samplePath);
            var lines = File.ReadAllLines(_inputSource switch
            {
                ProblemInputSource.Input => inputPath,
                ProblemInputSource.Sample => samplePath,
            });

            if (_runPartOne) RunProblem("⭐", () => problem.PartOne(lines)?.ToString());
            if (_runPartTwo) RunProblem("⭐⭐", () => problem.PartTwo(lines)?.ToString());
        }

        private string FilePath(Problem problem, string prefix)
        {
            var inputDir = Environment.GetEnvironmentVariable("INPUT_DIR") ?? Environment.CurrentDirectory;
            var path = $"{inputDir}/{prefix}-{problem.Year}-{problem.Day}.txt";
            return path;
        }

        private static void EnsureFileExists(string path)
        {
            if (File.Exists(path)) return;
            using var file = File.Create(path);
            Console.Out.WriteLine($"Created file at {path}");
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