using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days
{
    public class Advent202004 : Problem
    {
        public Advent202004() : base(2020, 04)
        {
        }

        public override string PartOne(string[] input)
        {
            var passports = ParsePassports(input).ToList();
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

            return passports.Count(passport => required.All(passport.ContainsKey)).ToString();
        }

        private static IEnumerable<Dictionary<string, string>> ParsePassports(IEnumerable<string> lines)
        {
            var currentFields = new Dictionary<string, string>();
            foreach (var line in lines)
            {
                if (line == "")
                {
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

            yield return currentFields;
        }

        public override string PartTwo(string[] input)
        {
            var passports = ParsePassports(input).ToList();
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
                    "hcl", s => Regex.IsMatch(s, @"^#[0123456789abcdef]+$")
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
                required.All(passport.ContainsKey) && passport.All(kv => validators[kv.Key](kv.Value))).ToString();
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