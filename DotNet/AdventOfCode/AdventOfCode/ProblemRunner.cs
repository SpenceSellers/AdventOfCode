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
            EnsureFileExists(inputPath, () => FetchInput(problem) ?? Array.Empty<byte>());
            EnsureFileExists(samplePath, Array.Empty<byte>);
            var lines = File.ReadAllLines(_inputSource switch
            {
                ProblemInputSource.Input => inputPath,
                ProblemInputSource.Sample => samplePath,
            });

            if (_runPartOne) RunProblem("⭐", () => problem.PartOne(lines)?.ToString());
            if (_runPartTwo) RunProblem("⭐⭐", () => problem.PartTwo(lines)?.ToString());
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

        private byte[] FetchInput(Problem problem)
        {
            if (SessionToken == null)
            {
                return null;
            }
            try
            {
                using var client = new HttpClient();
                var request = new HttpRequestMessage();
                request.Headers.Add("Cookie", $"session={SessionToken}");
                request.Method = HttpMethod.Get;
                request.RequestUri = new Uri($"https://adventofcode.com/{problem.Year}/day/{problem.Day}/input");

                var response = client.Send(request);
                Console.Out.WriteLine("Fetched puzzle input");
                var bytes = response.Content.ReadAsByteArrayAsync().Result;
                if (Encoding.UTF8.GetString(bytes).Contains("Please don't repeatedly request this endpoint before it unlocks!"))
                {
                    Console.Out.WriteLine("Day has not started yet, creating empty file.");
                    return null;
                }
                return bytes;
            }
            catch (Exception e)
            {
                Console.Out.WriteLine("Failed to fetch puzzle input, creating empty file");
                return null;
            }
        }

        private static string? SessionToken => Environment.GetEnvironmentVariable("SESSION_TOKEN");
    }
}