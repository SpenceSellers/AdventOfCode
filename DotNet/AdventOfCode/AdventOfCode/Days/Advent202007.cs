
using System.Collections.Generic;
using System.Linq;
using System.Runtime;
using System.Security;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days
{
    public class Advent202007 : Problem
    {
        public Advent202007() : base(2020, 07)
        {
        }

        public override string PartOne(string[] input)
        {
            var rules = input.Select(ParseRule).ToList();


            var sum = rules
                .Select(rule => RecursiveBagsWhichCanContain(rules, rule.ParentName, "shiny gold"))
                .Count(c => c > 0); // We honestly don't care how many times we can contain a gold bag, we just care if we can at all.
            return sum.ToString();
        }

        private static int RecursiveBagsWhichCanContain(IList<Rule> rules, string parentName, string searchName)
        {
            var children = rules.First(x => x.ParentName == parentName).ContainsBags;
            var directCount = children.Count(child => child.Name == searchName);

            return directCount + children.Select(c => RecursiveBagsWhichCanContain(rules, c.Name, searchName)).Sum();
        }

        public override string PartTwo(string[] input)
        {
            var rules = input.Select(ParseRule).ToList();
            return RecursiveBagCount(rules, "shiny gold").ToString();
        }

        private static int RecursiveBagCount(IList<Rule> rules, string parentName)
        {
            var bags = rules.First(x => x.ParentName == parentName).ContainsBags;
            return bags.Select(bag => bag.Count + bag.Count * RecursiveBagCount(rules, bag.Name)).Sum();
        }

        private static Rule ParseRule(string rule)
        {
            var r = new Regex(@"(.*) bags contain (.*).");
            var match = r.Match(rule);
            var parent = match.Groups[1].Value;
            var childString = match.Groups[2].Value;
            
            var containmentRegex = new Regex(@"(\d+) (.*?) bags?");

            var contains = childString switch
            {
                "no other bags" => Enumerable.Empty<Containment>(),
                _ => childString
                    .Split(",")
                    .Select(s => s.Trim())
                    .Select(s =>
                    {
                        var containmentMatch = containmentRegex.Match(s);
                        return new Containment
                        {
                            Count = int.Parse(containmentMatch.Groups[1].Value),
                            Name = containmentMatch.Groups[2].Value
                        };
                    })
            };

            return new Rule
            {
                ParentName = parent,
                ContainsBags = contains.ToList()
            };
        }

        private static IEnumerable<string> BagsWhichCanContain(IList<Rule> rules, string child)
        {
            return rules.Where(rule => rule.ContainsBags.Any(containment => containment.Name == child))
                .Select(rule => rule.ParentName);
        }
        
        private class Rule
        {
            public string ParentName;
            public List<Containment> ContainsBags;
        }

        private class Containment
        {
            public int Count;
            public string Name;
        }
    }
    
}