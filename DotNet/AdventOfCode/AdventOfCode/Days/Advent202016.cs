using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days
{
    public class Advent202016 : Problem
    {
        public Advent202016() : base(2020, 16)
        {
        }

        public override string PartOne(string[] input)
        {
            var tickets = Parse(input);
            return null;
        }

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }

        private TicketSet Parse(string[] input)
        {
            var chunks = input
                .Where(line => line.Length != 0)
                .SplitList(line => line == "your ticket:" || line == "nearby tickets:")
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
        }
    }
}