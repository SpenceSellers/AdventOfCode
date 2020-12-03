using System;
using System.IO;

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

    }
}