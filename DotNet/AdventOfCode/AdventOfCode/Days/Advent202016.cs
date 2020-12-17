using System.Collections.Generic;

namespace AdventOfCode.Days
{
    public class Advent202016 : Problem
    {
        public Advent202016() : base(2020, 16)
        {
        }

        public override string PartOne(string[] input)
        {
            throw new System.NotImplementedException();
        }

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }

        private TicketSet Parse(string[] input)
        {
            return null;
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