using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days
{
    public class Advent202004 : Problem
    {
        public Advent202004() : base(2020, 04)
        {
        }

        public override object PartOne(string[] input)
        {
            return ParsePassports(input)
                .Count(HasRequiredFields);
        }

        public override object PartTwo(string[] input)
        {
            var passports = ParsePassports(input).ToList();

            var validators = new Dictionary<string, Func<string, bool>>
            {
                {
                    "byr", RangeValidator(1920, 2002)
                },
                {
                    "iyr", RangeValidator(2010, 2020)
                },
                {
                    "eyr", RangeValidator(2020, 2030)
                },
                {
                    "hgt", s =>
                    {
                        var match = Regex.Match(s, @"(\d+)(in|cm)");
                        if (!match.Success) return false;
                        var unit = match.Groups[2].Value;
                        var value = int.Parse(match.Groups[1].Value);
                        return unit switch
                        {
                            "in" => value >= 59 && value <= 76,
                            "cm" => value >= 150 && value <= 193,
                            _ => false // Invalid unit
                        };
                    }
                },
                {
                    "hcl", s => Regex.IsMatch(s, @"^#[0-9a-f]+$")
                },
                {
                    "ecl", s => new[] {"amb", "blu", "brn",  "gry", "grn", "hzl", "oth"}.Contains(s)
                },
                {
                    "pid", s => Regex.IsMatch(s, @"^\d{9}$")
                },
                {
                    // Valid but ignored
                    "cid", s => true
                }
            };
            return passports.Count(passport =>
                HasRequiredFields(passport) && passport.All(kv => validators[kv.Key](kv.Value))).ToString();
        }
        
        private static IEnumerable<Dictionary<string, string>> ParsePassports(IEnumerable<string> lines)
        {
            var currentFields = new Dictionary<string, string>();
            foreach (var line in lines)
            {
                if (line == "")
                {
                    // We're at the end of a passport
                    yield return currentFields;
                    currentFields = new Dictionary<string, string>();
                }
                else
                {
                    foreach (var kvp in line.Split(" ").Select(kvp => kvp.Split(":")))
                    {
                        currentFields.Add(kvp[0], kvp[1]);
                    }
                }
            }

            // Spit out the last passport even if there was no blank line
            if (currentFields.Any())
            {
                yield return currentFields;
            }
        }


        private static bool HasRequiredFields(IDictionary<string, string> passport)
        {
            var required = new HashSet<string>
            {
                "byr",
                "iyr",
                "eyr",
                "hgt",
                "hcl",
                "ecl",
                "pid"
            };

            return required.All(passport.ContainsKey);
        }

        private static Func<string, bool> RangeValidator(int min, int max)
        {
            return s =>
            {
                var num = int.Parse(s);
                return num >= min && num <= max;
            };
        }
    }
}