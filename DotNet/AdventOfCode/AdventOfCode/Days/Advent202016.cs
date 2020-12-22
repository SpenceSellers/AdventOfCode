using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days
{
    public class Advent202016 : Problem
    {
        public Advent202016() : base(2020, 16)
        {
        }

        public override object PartOne(string[] input)
        {
            var tickets = Parse(input);
            return tickets.AllInvalidValues().Sum();
        }

        public override object PartTwo(string[] input)
        {
            var tickets = Parse(input);
            var fields = tickets.LearnFields();
            var fieldsWeCareAbout = fields.Where(kv => kv.Key.StartsWith("departure")).Select(kv => kv.Value).ToList();

            var product = new BigInteger(1);
            foreach (var num in fieldsWeCareAbout.Select(i => tickets.MyTicket[i]))
            {
                product *= num;
            }

            return product;
        }

        private TicketSet Parse(string[] input)
        {
            var chunks = input
                .Where(line => line.Length != 0)
                .SplitList(line => line is "your ticket:" or "nearby tickets:")
                .ToList();

            var ruleChunk = chunks[0];
            var myTicketChunk = chunks[1][0].Split(',').Select(int.Parse).ToArray();
            var nearbyTicketsChunk = chunks[2].Select(l => l.Split(',').Select(int.Parse).ToArray()).ToArray();

            var ruleRegex = new Regex(@"(.*?): (\d+)-(\d+) or (\d+)-(\d+)");
            var rules = new Dictionary<string, List<(int, int)>>();
            foreach (var line in ruleChunk)
            {
                var match = ruleRegex.Match(line);
                var ruleBody = new List<(int, int)>
                {
                    (int.Parse(match.Groups[2].Value), int.Parse(match.Groups[3].Value)),
                    (int.Parse(match.Groups[4].Value), int.Parse(match.Groups[5].Value)),
                };
                
                rules.Add(match.Groups[1].Value, ruleBody);
            }
            
            return new TicketSet(rules, myTicketChunk, nearbyTicketsChunk);
        }

        private class TicketSet
        {
            public Dictionary<string, List<(int, int)>> Rules { get; }
            public int[] MyTicket { get; }
            public int[][] NearbyTickets { get; }

            public TicketSet(Dictionary<string, List<(int, int)>> rules, int[] myTicket, int[][] nearbyTickets)
            {
                Rules = rules;
                MyTicket = myTicket;
                NearbyTickets = nearbyTickets;
            }

            public IEnumerable<int> AllInvalidValues()
            {
                return NearbyTickets.SelectMany(x => x).Where(n => !AllRanges().Any(r => ValidForRange(n, r)));
            }

            private IEnumerable<int[]> ValidTickets()
            {
                // Where ALL fields on the ticket match ANY range
                return NearbyTickets.Where(t => t.All(n => AllRanges().Any(r => ValidForRange(n, r))));
            }

            public IDictionary<string, int> LearnFields()
            {
                var columns = FieldColumns().ToList();

                var possibilities = new Dictionary<int, HashSet<string>>();
                
                // Figure out which rules can apply to each column, right out of the gate.
                for (var i = 0; i < columns.Count; i++)
                {
                    var column = columns[i];
                    var matchingRules = Rules
                        .Where(kv => column.All(n => kv.Value.Any(r => ValidForRange(n, r))))
                        .Select(kv => kv.Key);
                    
                    possibilities.Add(i, matchingRules.ToHashSet());
                }
                
                // Begin reduction
                var solidified = new HashSet<int>();
                while (true)
                {
                    // What have we not simplified?
                    var simplificationOptions = possibilities.Where(kv => kv.Value.Count == 1 && !solidified.Contains(kv.Key)).ToList();
                    if (!simplificationOptions.Any())
                    {
                        // There's nothing left to simplify
                        break;
                    }

                    var (singleKey, singleRuleName) = simplificationOptions.First();
                    
                    // Since we know for sure which column this rule belongs to, remove it from all the other columns.
                    foreach (var (_, v) in possibilities.Where(kv => kv.Key != singleKey))
                    {
                        v.Remove(singleRuleName.First());
                    }

                    solidified.Add(singleKey);
                }
                
                // Build answer
                return possibilities.ToDictionary(x => x.Value.First(), x => x.Key);
            }

            private IEnumerable<IList<int>> FieldColumns()
            {
                var length = NearbyTickets[0].Length;
                var validTickets = ValidTickets().ToList();
                Debug.Assert(validTickets[0].Length == length);
                for (var i = 0; i < length; i++)
                {
                    yield return validTickets.Select(ticket => ticket[i]).ToList();
                }
            }

            private IEnumerable<(int, int)> AllRanges()
            {
                return Rules.Values.SelectMany(r => r);
            }

            private bool ValidForRange(int n, (int, int) range)
            {
                var (min, max) = range;
                Debug.Assert(min < max);
                return n >= min && n <= max;
            }
        }
    }
}