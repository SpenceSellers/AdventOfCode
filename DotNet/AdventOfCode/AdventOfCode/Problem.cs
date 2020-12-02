using System;
using System.IO;
using System.Net;

namespace AdventOfCode
{
    public abstract class Problem
    {
        public readonly int Year;
        public readonly int Day;
        
        public Problem(int year, int day)
        {
            Year = year;
            Day = day;

        }
        
        public abstract string PartOne(string[] input);
        public abstract string PartTwo(string[] input);

        public void Run()
        {
            var inputDir = Environment.GetEnvironmentVariable("INPUT_DIR");
            var path = $"{inputDir}/input-{Year}-{Day}.txt";
            var lines = File.ReadAllLines(path);

            try
            {
                var result = PartOne(lines);
                Console.WriteLine($"⭐ {result}");
            }
            catch (NotImplementedException e)
            {
                
            }
            
            try
            {
                var result = PartTwo(lines);
                Console.WriteLine($"⭐⭐ {result}");
            }
            catch (NotImplementedException e)
            {
                
            }
        }
    }
}