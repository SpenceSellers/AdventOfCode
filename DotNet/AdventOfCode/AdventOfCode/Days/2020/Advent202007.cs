using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days._2020
{
    public class Advent202007 : Problem
    {
        public Advent202007() : base(2020, 07)
        {
        }

        public override object PartOne(string[] input)
        {
            var rules = input.Select(ParseRule).ToList();
            var bagRules = new BagsRules(rules);

            return rules
                .Count(rule => bagRules.BagCanContain(rule.ParentName, "shiny gold"));
        }


        public override object PartTwo(string[] input)
        {
            var rules = input.Select(ParseRule).ToList();
            return new BagsRules(rules).BagsInside("shiny gold");
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

        private class Rule
        {
            public string ParentName;
            public List<Containment> ContainsBags;
        }

        private record Containment
        {
            public int Count;
            public string Name;
        }

        private class BagsRules
        {
            private readonly List<Rule> _rules;

            public BagsRules(List<Rule> rules)
            {
                _rules = rules;
            }

            private Rule RuleForBag(string name) => _rules.First(r => r.ParentName == name);

            public int BagsInside(string name)
            {
                var bags = RuleForBag(name).ContainsBags;
                return bags.Select(bag => bag.Count + bag.Count * BagsInside(bag.Name)).Sum();
            }
            
            public bool BagCanContain(string parentName, string searchName)
            {
                var children = RuleForBag(parentName).ContainsBags;
                // If we directly contain it or any of our children contain it
                return children.Any(child => child.Name == searchName) || children.Any(c => BagCanContain(c.Name, searchName));
            }
        }
    }
}