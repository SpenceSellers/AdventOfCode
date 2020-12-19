using System;
using System.IO;
using System.Text.RegularExpressions;

namespace AdventOfCode
{
    public abstract class Problem
    {
        public readonly int Year;
        public readonly int Day;

        protected Problem(int year, int day)
        {
            Year = year;
            Day = day;
        }

        protected Problem()
        {
            // Infer the day from the class name.
            // Are you horrified?
            
            var className = GetType().Name;
            Console.Out.WriteLine($"We are {className}");

            var match = new Regex(@"\D*(\d{4})(\d{2})").Match(className);
            Year = int.Parse(match.Groups[1].Value);
            Day = int.Parse(match.Groups[2].Value);
        }
        
        public abstract string PartOne(string[] input);
        public abstract string PartTwo(string[] input);

    }
}