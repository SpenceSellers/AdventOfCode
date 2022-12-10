using System;
using System.Diagnostics;
using System.IO;
using System.Net.Http;
using System.Text;

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
        private int _timesToRun = 1;
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

        public ProblemRunner RunTimes(int runTimes)
        {
            _timesToRun = runTimes;
            return this;
        }

        public void Run(Problem problem)
        {
            var inputPath = FilePath(problem, "input");
            var samplePath = FilePath(problem, "sample");
            EnsureFileExists(inputPath, () => new PuzzleFetcher(problem.Year, problem.Day).FetchInput() ?? Array.Empty<byte>());
            EnsureFileExists(samplePath, Array.Empty<byte>);
            var lines = File.ReadAllLines(_inputSource switch
            {
                ProblemInputSource.Input => inputPath,
                ProblemInputSource.Sample => samplePath,
            });
            
            Console.Out.WriteLine($"Running {problem.Year}-{problem.Day}");

            if (_runPartOne) RunProblem("⭐", () => problem.PartOne(lines)?.ToString());
            if (_runPartTwo) RunProblem("⭐⭐", () => problem.PartTwo(lines)?.ToString());
        }

        private void RunProblem(string title, Func<string> func)
        {
            try
            {
                var stopwatch = new Stopwatch();
                stopwatch.Start();
                object result = null;
                for (int i = 0; i < _timesToRun; i++)
                {
                    result = func();
                }
                stopwatch.Stop();
                Console.WriteLine($"== {title} ==");
                Console.WriteLine($"Complete in {stopwatch.ElapsedMilliseconds} ms");
                if (_timesToRun != 1)
                {
                    var average = (double)stopwatch.ElapsedMilliseconds / _timesToRun;
                    Console.Out.WriteLine($"Across {_timesToRun} runs (Mean {average} ms)");
                }
                Console.WriteLine(result);
            }
            catch (NotImplementedException e)
            {

            }
        }

        private string FilePath(Problem problem, string prefix)
        {
            var inputDir = Environment.GetEnvironmentVariable("INPUT_DIR") ?? Environment.CurrentDirectory;
            var path = $"{inputDir}/{prefix}-{problem.Year}-{problem.Day}.txt";
            return path;
        }

        private static void EnsureFileExists(string path, Func<byte[]> content)
        {
            if (File.Exists(path)) return;
            using var file = File.Create(path);
            file.Write(content());
            Console.Out.WriteLine($"Created file at {path}");
        }

    }
}